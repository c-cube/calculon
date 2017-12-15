open Prelude
open Containers

module J = Yojson.Safe.Util
type json = Yojson.Safe.json

type to_tell = {
  from: string;
  on_channel: string;
  msg: string;
  tell_after: float option; (** optional; not before this deadline (UTC) *)
}

(* Data for contacts *)
type contact = {
  last_seen: float;
  to_tell: to_tell list;
  ignore_user: bool;    (* user does not turn up in searches etc. *)
}

exception Bad_json

let contact_of_json (json: json): contact option =
  let member k =
    match J.member k json with
    | `Null -> raise Bad_json
    | v -> v in
  try
    { last_seen = member "lastSeen" |> J.to_float;
      to_tell =
        member "to_tell"
        |> J.convert_each (fun j ->
          match J.convert_each J.to_string j with
            | [from; on_channel; msg] -> {from; on_channel; msg; tell_after=None}
            | [from; on_channel; msg; tell_after] ->
              let tell_after = Some (float_of_string tell_after) in
              {from; on_channel; msg; tell_after;}
            | _ -> raise Bad_json);
      ignore_user = match J.member "ignore_user" json with
        | `Null -> false;
        | v -> J.to_bool_option v
               |> CCOpt.get_or ~default:false
    } |> some
  with Bad_json | J.Type_error (_, _) -> None

let json_of_contact (c: contact): json =
  `Assoc [
    "lastSeen", `Float c.last_seen;
    "to_tell", `List (
      List.map (fun {from; on_channel; msg; tell_after} ->
        let last = match tell_after with
          | None -> []
          | Some f -> [`String (string_of_float f)]
        in
        `List ([`String from; `String on_channel; `String msg] @ last)
      ) c.to_tell
    );
    "ignore_user", `Bool c.ignore_user
  ]

(* Contacts db *)

type state = {
  actions: Plugin.action_callback;
  mutable map: contact StrMap.t;
}

let write_db (db:state) =
  Signal.Send_ref.send db.actions Plugin.Require_save

let is_contact state nick = StrMap.mem nick state.map

let set_data state ?(force_sync = true) nick contact =
  state.map <- StrMap.add nick contact state.map;
  if force_sync then Lwt.async (fun () -> write_db state)

let new_contact state nick =
  if not (is_contact state nick) then
    set_data state nick {
      last_seen = Unix.time ();
      to_tell = [];
      ignore_user = false;
    }

let data state nick =
  if not @@ is_contact state nick then new_contact state nick;
  StrMap.find nick state.map

let split_2 ~msg re s =
  let a = Re.split re s in
  match a with
    | x :: y -> x, String.concat " " y
    | _ -> raise (Command.Fail msg)

let split_3 ~msg re s =
  let a = Re.split re s in
  match a with
    | x::y::tail -> x,y,String.concat " " tail
    | _ -> raise (Command.Fail msg)

let cmd_tell_inner ~at state =
  Command.make_simple
    ~descr:("ask the bot to transmit a message to someone absent\n"
      ^ if at then "format: <date> <nick> <msg>" else "format: <nick> <msg>")
    ~prio:10 ~prefix:(if at then "tell_at" else "tell")
    (fun msg s ->
       let nick = msg.Core.nick in
       let target = Core.reply_to msg in
       let s = String.trim s in
       try
         let dest, msg, tell_after =
           if at
           then (
             let d, m, t =
               split_3 ~msg:"tell_at: expected <date> <nick> <msg>"
                 (Re_perl.compile_pat "[ \t]+") s
             in
             let t = ISO8601.Permissive.datetime ~reqtime:false t in
             d, m, Some t
           ) else (
             let d, m =
               split_2 ~msg:"tell: expected <nick> <msg>"
                 (Re_perl.compile_pat "[ \t]+") s
             in
             d, m, None
           )
         in
         set_data state dest
           {(data state dest) with
              to_tell =
                {from=nick; on_channel=target; msg; tell_after}
                :: (data state dest).to_tell};
         Lwt.return_some (Talk.select Talk.Ack)
       with
         | Command.Fail _ as e -> Lwt.fail e
         | e -> Lwt.fail (Command.Fail ("tell: " ^ Printexc.to_string e))
    )

let cmd_tell = cmd_tell_inner ~at:false
let cmd_tell_at = cmd_tell_inner ~at:true

(* human readable display of date *)
let print_diff (f:float) : string =
  let spf = Printf.sprintf in
  let s = Pervasives.mod_float f 60. |> int_of_float in
  let m = Pervasives.mod_float (f /. 60.) 60. |> int_of_float in
  let h = Pervasives.mod_float (f /. 3600.) 24. |> int_of_float in
  let days = f /. (3600. *. 24.) |> int_of_float in
  [ (if days > 0 then [spf "%d days" days] else []);
    (if h > 0 then [spf "%d hours" h] else []);
    (if m > 0 then [spf "%d minutes" m] else []);
    [spf "%d seconds" s];
  ] |> List.flatten |> String.concat ", "


let create_message_for_user now (user,last) =
  let diff = now -. last in
  CCFormat.sprintf "seen %s last: %s ago" user (print_diff diff)

let cmd_seen (state:state) =
  Command.make_simple_l
    ~descr:"ask for the last time someone talked on this chan"
    ~prio:10 ~prefix:"seen"
    (fun _msg s ->
       try
         let dest = CCString.trim s |> CCString.uppercase_ascii in
         Log.logf "query: seen `%s`" dest;
         let now = Unix.time () in
         StrMap.fold (fun name data acc ->
             if dest = String.uppercase name then
               (name, data.last_seen) :: acc
             else
               acc )
           state.map []
         |> CCList.sort (fun a b -> - (compare (snd a) (snd b)) )
         |> CCList.map (create_message_for_user now)
         |> Lwt.return
       with e ->
         Lwt.fail (Command.Fail ("seen: " ^ Printexc.to_string e)))


let cmd_last (state:state) =
  Command.make_simple_l
    ~descr:"ask for the last n people talking on this chan (default: n=3)"
    ~prio:10 ~prefix:"last"
    (fun msg s ->
       try
         let default_n = 3 in
         let dest = String.trim s in
         Log.logf "query: last `%s`" dest;
         let top_n = try match int_of_string dest with
           | x when x > 0 -> x
           | _ -> default_n
           with
           | Failure _ -> default_n
         in
         let now = Unix.time () in
         let user_times =
           StrMap.fold (fun key contact acc ->
               if key != msg.nick && contact.ignore_user |> not then
                 (key, contact.last_seen) :: acc
               else
                 acc
             )
             state.map []
           |> CCList.sort (fun a b -> - (compare (snd a) (snd b)) )
           |> CCList.tl (* remove person who asked *)
           |> CCList.take top_n
           |> CCList.map (create_message_for_user now)
         in
         Lwt.return user_times
       with e ->
         Lwt.fail (Command.Fail ("last_seen: " ^ Printexc.to_string e)))

let cmd_ignore_template ~prefix prefix_stem ignore (state:state) =
  Command.make_simple
    ~descr:(prefix ^ " nick")
    ~prio:10 ~prefix
    (fun msg s ->
       try
         let dest = String.trim s in
         Log.logf "query: ignore `%s`" dest;
         if dest = ""
         then Lwt.return None
         else (
           let contact = data state dest in
           let msg =
             if contact.ignore_user = ignore then
               CCFormat.sprintf "already %sing %s" prefix_stem dest |> some
             else (
               set_data ~force_sync:true state dest
                 { contact with ignore_user = ignore };
               CCFormat.sprintf "%sing %s" prefix_stem dest |> some )
           in
           Lwt.return msg )
       with e ->
         Lwt.fail (Command.Fail (prefix ^ ": " ^ Printexc.to_string e)))

let cmd_ignore = cmd_ignore_template ~prefix:"ignore" "ignor" true
let cmd_unignore = cmd_ignore_template ~prefix:"unignore" "unignor" false

let cmd_ignore_list (state:state) =
  Command.make_simple_l
    ~descr:"add nick to list of ignored people"
    ~prio:10 ~prefix:"ignore_list"
    (fun msg s ->
       try
         Log.logf "query: ignore_list";
         let ignored =
           StrMap.fold (fun name -> function
               | { ignore_user = true; _ } -> fun x -> name :: x
               | (* ignore_user = false; *) _ -> fun x -> x
             ) state.map [] in
         let msg =
           if CCList.is_empty ignored
           then ["noone ignored!"]
           else "ignoring:" :: ignored
         in
         Lwt.return msg
       with e ->
         Lwt.fail (Command.Fail ("ignore_list: " ^ Printexc.to_string e)))


(* callback to update state, notify users of their messages, etc. *)
let on_message state (module C:Core.S) msg =
  let module Msg = Irc_message in
  let nick = match msg.Msg.command with
    | Msg.JOIN (_, _) | Msg.PRIVMSG (_, _) ->
      some @@ get_nick @@ Option.get_exn msg.Msg.prefix
    | Msg.NICK newnick ->
      Some newnick
    | _ -> None
  in
  (* trigger [tell] messages *)
  begin match nick with
    | None -> Lwt.return ()
    | Some nick ->
      (* update [lastSeen] *)
      set_data state ~force_sync:false nick
        {(data state nick) with last_seen = Unix.time ()};
      let contact = data state nick in
      let to_tell, remaining =
        let now = Unix.time() in
        contact.to_tell
        |> List.partition
          (fun t -> match t.tell_after with
             | None -> true
             | Some f when now > f -> true
             | Some _ -> false)
      in
      if to_tell <> [] then (
        set_data state nick {contact with to_tell = remaining};
      );
      Lwt_list.iter_s (fun {from=author; on_channel; msg=m; _} ->
        C.send_notice ~target:on_channel
          ~message:(Printf.sprintf "%s: (from %s): %s" nick author m))
        (List.rev to_tell)
  end

let of_json actions = function
  | None ->
    Lwt_err.return {actions; map=StrMap.empty; }
  | Some j ->
    let map = match j with
      | `Assoc l ->
        l
        |> CCList.filter_map (fun (k, j) ->
          Option.(contact_of_json j >>= fun c -> Some (k, c)))
        |> StrMap.of_list
      | _ -> StrMap.empty
    in
    Lwt_err.return {actions; map}

let to_json (db:state) =
  let json = `Assoc (
    StrMap.to_list db.map
    |> List.map (fun (k, c) -> (k, json_of_contact c))
  ) in
  Some json

let plugin =
  let commands state =
    [ cmd_tell state;
      cmd_tell_at state;
      cmd_seen state;
      cmd_last state;
      cmd_ignore state;
      cmd_unignore state;
      cmd_ignore_list state;
    ]
  in
  Plugin.stateful
    ~name:"social"
    ~on_msg:(fun st -> [on_message st])
    ~of_json ~to_json ~commands ()
