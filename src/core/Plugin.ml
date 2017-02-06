(** {1 Plugins} *)

type json = Yojson.Safe.json

type action =
  | Require_reload
  (** Require that we reload everything from on-disk state *)
  | Require_save
  (** Require that the state be saved *)

type action_callback = action Signal.Send_ref.t

type stateful = St : 'st stateful_ -> stateful

and 'st stateful_ = {
  name: string;
  (* namespace for storing state. Must be distinct for every plugin. *)
  commands: 'st -> Command.t list;
  (* commands parametrized by some (mutable) state, with the ability
     to trigger a signal *)
  on_msg:'st -> (Core.t -> Irc_message.t -> unit Lwt.t) list;
  (* executed on each incoming message *)
  to_json : 'st -> json option;
  (* how to serialize (part of) the state into JSON, if need be. *)
  of_json : action_callback -> json option -> ('st, string) Result.result Lwt.t;
  (* how to deserialize the state. [None] is passed for a fresh
     initialization *)
  stop: 'st -> unit Lwt.t;
  (* stop the plugin.
     It is NOT the responsibility of this command to save the state,
     as the core engine will have called {!to_json} before. *)
}

(** A single plugin *)
type t =
  | Stateful of stateful
  | Stateless of Command.t list

type plugin = t

let of_cmd c = Stateless [c]
let of_cmds l =
  if l=[] then invalid_arg "Plugin.of_cmds";
  Stateless l

let stateful
    ~name
    ~commands
    ?(on_msg=fun _ -> [])
    ~to_json
    ~of_json
    ?(stop=fun _ -> Lwt.return_unit)
    () =
  Stateful (St { name; on_msg; to_json; of_json; stop; commands; })

(** {2 Collection of Plugins} *)
module Set = struct
  type active_plugin =
    | Active_stateful : 'st stateful_ * 'st -> active_plugin
    | Active_stateless of Command.t list

  type t = {
    config: Config.t;
    plugins: plugin list;
    actions: action Signal.t;
    mutable active : active_plugin list;
    mutable commands_l: Command.t list; (* sorted by prio *)
    mutable on_msg_l: (Core.t -> Irc_message.t -> unit Lwt.t) list;
    mutable stopped: bool;
  }

  (* "safe" writing to file, using a temporary file + atomic move *)
  let save_state_ config j: unit =
    let file = config.Config.state_file in
    Log.logf "plugin: save state in '%s'" file;
    let file' = file ^ ".tmp" in
    try
      Yojson.Safe.to_file file' j;
      try Sys.rename file' file
      with e ->
        Log.logf "failed to save into '%s' (temp file '%s'): %s"
          file file' (Printexc.to_string e)
    with e ->
      Log.logf "failed to write into temp file '%s': %s"
        file' (Printexc.to_string e)

  let save_ config active =
    let assoc_l =
      List.fold_left
        (fun acc p -> match p with
           | Active_stateless _ -> acc
           | Active_stateful (plugin, state) ->
             match plugin.to_json state with
               | None -> acc
               | Some j ->
                 (plugin.name, j) :: acc)
        [] active
    in
    let j = `Assoc assoc_l in
    save_state_ config j;
    Lwt.return_unit

  let save t =
    save_ t.config t.active

  let load_state_ config : (json, string) Result.result Lwt.t =
    let file = config.Config.state_file in
    Log.logf "load from file '%s'" file;
    if Sys.file_exists file then (
      try
        let j = Yojson.Safe.from_file file in
        Lwt_err.return j
      with e ->
        Lwt_err.fail
          (Printf.sprintf "could not load state file '%s': %s" file (Printexc.to_string e))
    ) else Lwt_err.return (`Assoc[])

  let commands t = t.commands_l
  let on_msg_l t = t.on_msg_l

  (* fold map on lwt+err monad *)
  let fold_map_l_err f acc l =
    let open Lwt_err in
    let rec aux acc elts l = match l with
      | [] -> return (acc, List.rev elts)
      | x :: tail ->
        f acc x >>= fun (acc,y) -> aux acc (y::elts) tail
    in
    aux acc [] l

  let load_from action_signal plugins (j:json)
    : (Command.t list * _ list * active_plugin list) Lwt_err.t =
    let open Lwt_err in
    begin match j with
      | `Assoc l -> return l
      | _ -> fail ("state should be a JSON object")
    end >>= fun assoc_list ->
    (* initialize the plugins *)
    fold_map_l_err
      (fun (cmds_l,on_msg_l) p -> match p with
        | Stateful (St plugin) ->
          let name = plugin.name in
          let plugin_j =
            try Some (List.assoc name assoc_list) with Not_found -> None
          in
          (plugin.of_json action_signal plugin_j
            |> map_err (Printf.sprintf "in plugin '%s': %s" plugin.name))
          >|= fun state ->
          let cmds = plugin.commands state in
          let on_msg = plugin.on_msg state in
          (cmds @ cmds_l, on_msg @ on_msg_l), Active_stateful (plugin, state)
        | Stateless cmds ->
          return ((cmds @ cmds_l,on_msg_l), Active_stateless cmds)
      )
      ([],[])
      plugins
    >|= fun ((cmds,on_msg_l),active) ->
    let commands_l = List.sort Command.compare_prio cmds in
    commands_l,on_msg_l,active

  let reload t =
    let open Lwt_err in
    Log.log "plugin: reload state";
    load_state_ t.config >>= fun j ->
    load_from (Signal.Send_ref.make t.actions) t.plugins j
    >|= fun (commands, on_msg_l, active) ->
    t.commands_l <- commands;
    t.on_msg_l <- on_msg_l;
    t.active <- active;
    ()

  let save_period = 300.

  (* periodic "save" *)
  let save_thread t : unit Lwt.t =
    let open Lwt.Infix in
    let rec loop () =
      Lwt_unix.sleep save_period >>= fun () ->
      if t.stopped then Lwt.return ()
      else (
        save t >>= fun _ ->
        loop ()
      )
    in
    loop ()

  let create config (plugins:plugin list) : (t, string) Result.result Lwt.t =
    let open Lwt_err in
    load_state_ config >>= fun j ->
    let actions = Signal.create() in
    load_from (Signal.Send_ref.make actions) plugins j
    >|= fun (commands_l, on_msg_l, active) ->
    let t = {
      config; plugins; actions; active; commands_l; on_msg_l; stopped=false;
    } in
    (* respond to actions *)
    Signal.on' actions
      (function
        | Require_save -> save t
        | Require_reload -> Lwt.map ignore (reload t)
      );
    (* save thread *)
    Lwt.async (fun () -> save_thread t);
    t

  let stop ?save:(save_opt=true) t : unit Lwt.t =
    let open Lwt.Infix in
    if t.stopped then Lwt.return_unit
    else (
      t.stopped <- true;
      (if save_opt then save t else Lwt.return_unit) >>= fun () ->
      Lwt_list.iter_p
        (function
          | Active_stateless _ -> Lwt.return ()
          | Active_stateful (p, st) -> p.stop st)
        t.active
    )
end
