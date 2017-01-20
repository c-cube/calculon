open Prelude
open Containers
open Lwt.Infix

module J = Yojson.Basic.Util
type json = Yojson.Basic.json

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
  coucous : int;
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
      coucous = member "coucous" |> J.to_int_option |? 0
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
    "coucous", `Int c.coucous
  ]

(* Contacts db *)

type t = contact StrMap.t

(* TODO: move to config *)
let db_filename = "socialdb.json"

let read_db (): t =
  match Yojson.Basic.from_file db_filename with
  | `Assoc l ->
    l
    |> CCList.filter_map (fun (k, j) ->
      Option.(contact_of_json j >>= fun c -> Some (k, c)))
    |> StrMap.of_list
  | exception (Sys_error _) -> StrMap.empty
  | _ -> StrMap.empty

let write_db (db: t) =
  let json = `Assoc (
    StrMap.to_list db
    |> List.map (fun (k, c) -> (k, json_of_contact c))
  ) in
  Yojson.Basic.to_file db_filename json

type state = t ref

let is_contact state nick = StrMap.mem nick !state

let set_data state ?(force_sync = true) nick contact =
  state := StrMap.add nick contact !state;
  if force_sync then write_db !state

let sync state = write_db !state

let new_contact state nick =
  if not (is_contact state nick) then
    set_data state nick {
      last_seen = Unix.time ();
      to_tell = [];
      coucous = 0;
    }

let data state nick =
  if not @@ is_contact state nick then new_contact state nick;
  StrMap.find nick !state

(* plugin *)

(* Update coucous *)
let is_coucou msg =
  contains msg (Str.regexp "[^!]\\bcoucou\\b")
  ||
  CCString.prefix ~pre:"coucou" msg

let () =
  assert (is_coucou "coucou");
  assert (is_coucou " coucou");
  assert (is_coucou "foo bar coucou yolo");
  assert (not (is_coucou "!coucou"));
  assert (not (is_coucou "!coucou yolo"));
  ()

let shift_coucou ~by state nick =
  let d = data state nick in
  set_data state ~force_sync:false nick {d with coucous = d.coucous + by}

let incr_coucou = shift_coucou ~by:1
let decr_coucou = shift_coucou ~by:~-1

(* Write the db to the disk periodically.

   We do not update the on-disk db each time lastSeen is updated (i.e. each time
   someone talks), as it's not a big deal if we lose some data about lastSeen in
   case of a crash.
*)
let save_thread state =
  let rec loop () =
    Lwt_unix.sleep 300. >>= fun () ->
    sync state; loop ()
  in
  Lwt.async loop

let split_2 ~msg re s =
  let a = Str.bounded_split re s 2 in
  match a with
    | [x;y] ->x,y
    | _ -> raise (Command.Fail msg)

let split_3 ~msg re s =
  let a = Str.bounded_split re s 3 in
  match a with
    | [x;y;z] -> x,y,z
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
               split_3 ~msg:"tell_at: expected <date> <nick> <msg>" (Str.regexp " ") s
             in
             let t = ISO8601.Permissive.datetime ~reqtime:false t in
             d, m, Some t
           ) else (
             let d, m =
               split_2 ~msg:"tell: expected <nick> <msg>" (Str.regexp " ") s
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


let cmd_seen state =
  Command.make_simple
    ~descr:"ask for the last time someone talked on this chan"
    ~prio:10 ~prefix:"seen"
    (fun _msg s ->
       try
         let dest = String.trim s in
         Log.logf "query: seen `%s`" dest;
         begin match StrMap.get dest !state with
           | Some data ->
             let last = data.last_seen in
             let now = Unix.time () in
             let diff = now -. last in
             let msg =
               CCFormat.sprintf "seen %s last: %s ago" dest (print_diff diff)
             in
             Lwt.return_some msg
           | None ->
             Lwt.return_some "who?"
         end
       with e ->
         Lwt.fail (Command.Fail ("seen: " ^ Printexc.to_string e)))

let cmd_coucou state =
  Command.make_simple
    ~descr:"increment coucou level" ~prefix:"coucou" ~prio:10
    (fun msg s ->
       let s = String.trim s in
       if contains s (Str.regexp " ") then Lwt.return_none
       else
         let nick = if s <> "" then s else msg.Core.nick in
         let coucou_count = (data state nick).coucous in
         let message =
           Printf.sprintf "%s est un coucouteur niveau %d"
             nick coucou_count
         in
         Lwt.return (Some message)
    )

let cmd_reload state =
  Command.make_simple ~descr:"reload socialdb" ~prefix:"social_reload" ~prio:10
    (fun _ _ ->
       let new_db = read_db() in
       state := new_db;
       Lwt.return_some (Talk.select Talk.Ack)
    )

(* callback to update state, notify users of their messages, etc. *)
let on_message (module C:Core.S) state msg =
  let module Msg = Irc_message in
  let nick =
    match msg.Msg.command with
    | Msg.JOIN (_, _) | Msg.PRIVMSG (_, _) ->
      some @@ get_nick @@ Option.get_exn msg.Msg.prefix
    | Msg.NICK newnick ->
      Some newnick
    | _ -> None
  in
  match nick with
  | None -> Lwt.return ()
  | Some nick ->
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

let plugin =
  let init ((module C:Core.S) as core) _conf =
    let state = ref (read_db ()) in
    (* Update lastSeen *)
    Signal.on' C.privmsg
      (fun msg ->
         set_data state ~force_sync:false msg.Core.nick
           {(data state msg.Core.nick) with last_seen = Unix.time ()};
         Lwt.return ());
    (* update coucou *)
    Signal.on' C.privmsg
      (fun msg ->
         let target = Core.reply_to msg in
         if is_coucou msg.Core.message then (
           if Core.is_chan target
           then incr_coucou state msg.Core.nick
           else decr_coucou state msg.Core.nick
         );
         Lwt.return ());
    (* notify users *)
    Signal.on' C.messages (on_message core state);
    (* periodic save *)
    save_thread state;
    Lwt.return state
  and stop state =
    (* TODO: stop saving thread? *)
    write_db !state |> Lwt.return
  and commands state =
    [ cmd_tell state;
      cmd_tell_at state;
      cmd_coucou state;
      cmd_seen state;
      cmd_reload state;
    ]
  in
  Plugin.stateful ~init ~stop commands

