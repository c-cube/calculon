(** {1 Plugins} *)

open Db_utils
module Log = Core.Log

type json = Yojson.Safe.t

type action =
  | Require_reload  (** Require that we reload everything from on-disk state *)
  | Require_save  (** Require that the state be saved *)

type action_callback = action Signal.Send_ref.t

type stateful = St : 'st stateful_ -> stateful

and 'st stateful_ = {
  name: string;
      (** Namespace for storing state. Must be distinct for every plugin. *)
  commands: 'st -> Command.t list;
      (** Commands parametrized by some (mutable) state, with the ability to
          trigger a signal *)
  on_msg: 'st -> (Core.t -> Irky.Message.t -> unit) list;
      (** Executed on each incoming message *)
  to_json: 'st -> json option;
      (** How to serialize (part of) the state into JSON, if need be. *)
  of_json: action_callback -> json option -> ('st, string) result;
      (** How to deserialize the state. [None] is passed for a fresh
          initialization. *)
  stop: 'st -> unit;
      (** Stop the plugin. It is NOT the responsibility of this command to save
          the state, as the core engine will have called {!to_json} before. *)
}

type db_backed = {
  commands: DB.db -> Command.t list;
      (** Commands parametrized by some (mutable) state, with the ability to
          trigger a signal *)
  prepare_db: DB.db -> unit;
      (** Prepare database (create tables, etc.). Must be idempotent as it'll be
          called every time the plugin is initialized. *)
  on_msg: DB.db -> (Core.t -> Irky.Message.t -> unit) list;
      (** Executed on each incoming message *)
  stop: DB.db -> unit;
      (** Stop the plugin. There is no need to close the DB connection. *)
}

(** A single plugin *)
type t =
  | Stateful of stateful
  | Stateless of Command.t list
  | DB_backed of db_backed

type plugin = t

let of_cmd c = Stateless [ c ]

let of_cmds l =
  if l = [] then invalid_arg "Plugin.of_cmds";
  Stateless l

let stateful ~name ~commands ?(on_msg = fun _ -> []) ~to_json ~of_json
    ?(stop = fun _ -> ()) () =
  Stateful (St { name; on_msg; to_json; of_json; stop; commands })

let db_backed ~commands ~prepare_db ?(on_msg = fun _ -> [])
    ?(stop = fun _ -> ()) () : t =
  DB_backed { commands; prepare_db; on_msg; stop }

(* prepare the main plugin tables, settings *)
let prepare_db_ db =
  DB.busy_timeout db 500;
  DB.exec db
    {|
    CREATE TABLE IF NOT EXISTS plugins
      (name TEXT NOT NULL,
       value TEXT NOT NULL,
       UNIQUE (name) ON CONFLICT FAIL
       );
    |}
  |> check_db_ db;
  Printf.printf "creating index\n";
  DB.exec db
    {|
    CREATE INDEX IF NOT EXISTS plugins_idx on plugins(name);
    |}
  |> check_db_ db;
  ()

let unwrap_failwith = function
  | Ok x -> x
  | Error e -> failwith e

(** {2 Collection of Plugins} *)
module Set = struct
  type active_plugin =
    | Active_stateful : 'st stateful_ * 'st -> active_plugin
    | Active_stateless of Command.t list
    | Active_db_backed of db_backed

  type t = {
    config: Config.t;
    plugins: plugin list;
    actions: action Signal.t;
    db: DB.db;
    mutable active: active_plugin list;
    mutable commands_l: Command.t list; (* sorted by prio *)
    mutable on_msg_l: (Core.t -> Irky.Message.t -> unit) list;
    mutable stopped: bool;
  }

  let create_db config : DB.db =
    let db_file = config.Config.db_file in
    let db = DB.db_open db_file in
    prepare_db_ db;
    db

  (* save JSON plugins *)
  let save_ db _config active =
    DB.exec db "BEGIN;" |> check_db_ db;

    let save_plugin = function
      | Active_stateless _ | Active_db_backed _ -> ()
      | Active_stateful (plugin, state) ->
        (* save as json into the appropriate table *)
        (match plugin.to_json state with
        | None -> ()
        | Some j ->
          let@ stmt =
            with_stmt db
              {|INSERT OR REPLACE INTO plugins(name,value) VALUES(?,?);|}
          in
          DB.bind_text stmt 1 plugin.name |> check_db_ db;
          DB.bind_text stmt 2 (Yojson.Safe.to_string j) |> check_db_ db;
          DB.step stmt |> check_db_ db)
    in

    List.iter save_plugin active;
    DB.exec db "COMMIT;" |> check_db_ db

  let save (self : t) : unit = save_ self.db self.config self.active
  let commands t = t.commands_l
  let on_msg_l t = t.on_msg_l

  let load_from (db : DB.db) action_signal plugins (_config : Config.t) :
      (Command.t list * _ list * active_plugin list, _) result =
    guard_res @@ fun () ->
    let all_cmds = ref [] in
    let all_on_msg = ref [] in

    let init = function
      | Stateless cmds ->
        all_cmds := List.rev_append cmds !all_cmds;
        Active_stateless cmds
      | Stateful (St plugin) ->
        let plugin_j =
          let@ stmt =
            with_stmt db {|SELECT json(value) FROM plugins WHERE name=?|}
          in
          DB.bind_text stmt 1 plugin.name |> check_db_ db;
          DB.step stmt |> check_db_ db;
          try
            let j = DB.column_text stmt 1 in
            Some (Yojson.Safe.from_string j)
          with _ -> None
        in

        (match plugin.of_json action_signal plugin_j with
        | Error err ->
          failwith (spf "plugin %S failed to initialize: %s" plugin.name err)
        | Ok state ->
          all_cmds := List.rev_append (plugin.commands state) !all_cmds;
          all_on_msg := List.rev_append (plugin.on_msg state) !all_on_msg;

          Active_stateful (plugin, state))
      | DB_backed plugin ->
        plugin.prepare_db db;
        all_cmds := List.rev_append (plugin.commands db) !all_cmds;
        all_on_msg := List.rev_append (plugin.on_msg db) !all_on_msg;

        Active_db_backed plugin
    in

    let active = List.map init plugins in

    let commands_l = List.sort Command.compare_prio @@ !all_cmds in
    let on_msg_l = !all_on_msg in
    commands_l, on_msg_l, active

  let reload (self : t) : (unit, string) result =
    Log.info (fun k -> k "plugin: reload state");
    guard_res @@ fun () ->
    let commands, on_msg_l, active =
      load_from self.db
        (Signal.Send_ref.make self.actions)
        self.plugins self.config
      |> unwrap_failwith
    in
    self.commands_l <- commands;
    self.on_msg_l <- on_msg_l;
    self.active <- active

  let save_period = 300.

  (* periodic "save" - needs io for sleep *)
  let save_loop (io : Irky.Io.t) t : unit =
    let rec loop () =
      io.sleep save_period;
      if t.stopped then
        ()
      else (
        save t;
        loop ()
      )
    in
    loop ()

  let create ?cmd_help:(help = true) ~(io : Irky.Io.t) ~sw config
      (plugins : plugin list) : (t, string) result =
    guard_res @@ fun () ->
    let db = create_db config in
    let actions = Signal.create () in
    let commands_l, on_msg_l, active =
      load_from db (Signal.Send_ref.make actions) plugins config
      |> unwrap_failwith
    in
    let commands_l =
      if help then
        Command.cmd_help commands_l :: commands_l
      else
        commands_l
    in
    let self =
      {
        config;
        db;
        plugins;
        actions;
        active;
        commands_l;
        on_msg_l;
        stopped = false;
      }
    in
    (* respond to actions *)
    Signal.on' actions (function
      | Require_save -> save self
      | Require_reload -> ignore (reload self : (unit, string) result));
    (* save thread as Eio fiber *)
    Eio.Fiber.fork ~sw (fun () -> save_loop io self);
    self

  let stop ?(save : bool option) (self : t) : unit =
    let do_save =
      match save with
      | Some b -> b
      | None -> true
    in
    if not self.stopped then (
      Log.info (fun k -> k "stop plugins");
      self.stopped <- true;
      if do_save then save_ self.db self.config self.active;
      List.iter
        (function
          | Active_stateless _ -> ()
          | Active_db_backed p -> p.stop self.db
          | Active_stateful (p, st) -> p.stop st)
        self.active;
      (* close DB *)
      Log.info (fun k -> k "closing DB");
      while not (DB.db_close self.db) do
        ()
      done;
      Log.info (fun k -> k "all plugins stopped")
    )
end
