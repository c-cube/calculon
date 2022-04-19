open Prelude
open DB_utils
open Containers

module J = Yojson.Safe.Util
type json = Yojson.Safe.t

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

let equal_contact : contact -> contact -> bool = CCEqual.poly

(* we only need second precision here *)
let now = Unix.time

exception Bad_json of string

let contact_of_json (json: json): (contact, string) result =
  let member k =
    match J.member k json with
    | `Null -> raise (Bad_json  (spf "member not found: %S" k))
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
            | _ -> raise (Bad_json (spf "bad `tell` object: %s" (Yojson.Safe.to_string j))));
      ignore_user = match J.member "ignore_user" json with
        | `Null -> false;
        | v -> J.to_bool_option v
               |> Option.value ~default:false
    } |> (fun x->Ok x)
  with
  | Bad_json s -> Error s
  | J.Type_error (_, _) -> assert false

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

type t = DB.db

let prepare_db (self:t) : unit =
  DB.exec self
    {| CREATE TABLE IF NOT EXISTS
        social(name TEXT NOT NULL,
               value TEXT NOT NULL,
               UNIQUE (name) ON CONFLICT FAIL
               ) STRICT;
    |} |> check_db_ self;
  DB.exec self
    {| CREATE INDEX IF NOT EXISTS idx_social on social(name); |}
    |> check_db_ self;
  ()

let is_contact (self:t) nick: bool =
  let@ () = wrap_failwith "social.is_contact" in
  let@ stmt = with_stmt self {| SELECT EXISTS (SELECT * from social WHERE name=?) |} in
  DB.bind_text stmt 1 nick |> check_db_ self;
  DB.step stmt |> check_db_ self;
  DB.column_bool stmt 0

let set_data (self:t) nick (contact:contact) : unit =
  let nick = String.lowercase_ascii nick in
  let j = contact |> json_of_contact |> Yojson.Safe.to_string in
  let@ () = wrap_failwith "social.set_data" in
  let@ stmt =
    with_stmt self
      {| INSERT INTO social(name, value)
         VALUES(?1,?2)
         ON CONFLICT(name)
         DO UPDATE SET value=?2 |}
  in
  DB.bind_text stmt 1 nick |> check_db_ self;
  DB.bind_text stmt 2 j |> check_db_ self;
  DB.step stmt |> check_db_ self

let new_contact (self:t) nick : contact =
  let nick = String.lowercase_ascii nick in
  let d = {
    last_seen = now();
    to_tell = [];
    ignore_user = false;
  } in
  set_data self nick d;
  d

let data (self:t) nick : contact option =
  let nick = String.lowercase_ascii nick in
  let@ () = wrap_failwith "social.data" in
  if is_contact self nick then (
    let@ stmt =
      with_stmt self
        {| SELECT value FROM social WHERE name=? |}
    in
    DB.bind_text stmt 1 nick |> check_db_ self;
    DB.step stmt |> check_db_ self;
    match
      let s = DB.column_text stmt 0 in
      s, s |> Yojson.Safe.from_string |> contact_of_json
    with
    | _, Ok c -> Some c
    | s, Error err ->
      failwith (spf "invalid contact %S: %s" s err)
    | exception _ ->
      failwith (spf "cannot access contact %S" nick)
  ) else (
    None
  )

let ignored (self:t) : string list =
  let@ () = wrap_failwith "social.ignored" in
  let@ stmt =
    with_stmt self
      {| SELECT name FROM social
         WHERE json_extract(value, '$.ignore_user') = true |}
  in
  let rc, l =
    DB.fold stmt ~init:[] ~f:(fun acc row ->
      match row with
      | [| DB.Data.TEXT r |] -> r :: acc
      | _ -> acc)
  in
  check_db_ self rc; l

let last_talk (self:t) ~n : (string*float) list =
  let@ () = wrap_failwith "social.ignored" in
  let@ stmt =
    with_stmt self
      {| SELECT name, json_extract(value, '$.lastSeen') as lastSeen FROM social
         ORDER BY lastSeen DESC
         LIMIT ?|}
  in
  DB.bind_int stmt 1 n |> check_db_ self;
  let rc, l =
    DB.fold stmt ~init:[] ~f:(fun acc row ->
        match row with
        | [| DB.Data.TEXT r; DB.Data.FLOAT t |] -> (r,t) :: acc
        | _ -> acc)
  in
  check_db_ self rc; l

let data_or_insert (self:t) nick : contact =
  match data self nick with
  | Some c -> c
  | None -> new_contact self nick

let update_data (self:t) nick ~f : contact =
  let@ () = wrap_failwith "social.update-data" in
  let d = data_or_insert self nick in
  let d' = f d in
  if not (equal_contact d d') then set_data self nick d';
  d'

let update_data' self nick ~f : unit = ignore (update_data self nick ~f : contact)

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

let cmd_tell_inner ~at (self:t) =
  Command.make_simple
    ~descr:("ask the bot to transmit a message to someone absent\n"
      ^ if at then "format: <date> <nick> <msg>" else "format: <nick> <msg>")
    ~prio:10 ~cmd:(if at then "tell_at" else "tell")
    (fun msg s ->
       let nick = msg.Core.nick in
       let target = Core.reply_to msg in
       let s = String.trim s in
       try
         let dest, msg, tell_after =
           if at then (
             let d, m, t =
               split_3 ~msg:"tell_at: expected <date> <nick> <msg>"
                 (Re.Perl.compile_pat "[ \t]+") s
             in
             let t = match
                 Ptime.of_rfc3339 ~strict:false t
                 |> Ptime.rfc3339_error_to_msg
               with
               | Ok (t, _, _) -> Ptime.to_float_s t
               | Error (`Msg msg) ->
                 failwith (spf "invalid timestamp: %s" msg)
             in
             d, m, Some t
           ) else (
             let d, m =
               split_2 ~msg:"tell: expected <nick> <msg>"
                 (Re.Perl.compile_pat "[ \t]+") s
             in
             d, m, None
           )
         in
         update_data' self dest
           ~f:(fun state ->
               {state with to_tell =
                {from=nick; on_channel=target; msg; tell_after}
                :: state.to_tell});
         Some (Talk.select Talk.Ack)
       with
         | Command.Fail _ as e -> raise e
         | e -> raise (Command.Fail ("tell: " ^ Printexc.to_string e))
    )

let cmd_tell = cmd_tell_inner ~at:false
let cmd_tell_at = cmd_tell_inner ~at:true

(* human readable display of date *)
let print_diff (f:float) : string =
  let spf = Printf.sprintf in
  let s = mod_float f 60. |> int_of_float in
  let m = mod_float (f /. 60.) 60. |> int_of_float in
  let h = mod_float (f /. 3600.) 24. |> int_of_float in
  let days = mod_float (f /. (3600. *. 24.)) 365. |> int_of_float in
  let years = f /. (365. *. 3600. *. 24.) |> int_of_float in
  [ (if years>0 then [spf "%d years" years] else []);
    (if days > 0 then [spf "%d days" days] else []);
    (if h > 0 then [spf "%d hours" h] else []);
    (if m > 0 then [spf "%d minutes" m] else []);
    [spf "%d seconds" s];
  ] |> List.flatten |> String.concat ", "


let create_message_for_user now user last =
  let diff = now -. last in
  CCFormat.sprintf "seen %s last: %s ago" user (print_diff diff)

let cmd_seen (self:t) =
  Command.make_simple_l
    ~descr:"ask for the last time someone talked on this chan"
    ~prio:10 ~cmd:"seen"
    (fun _msg s ->
       try
         let now = now() in
         let nick = CCString.trim s |> String.lowercase_ascii in
         Logs.debug ~src:Core.logs_src (fun k->k "query: seen `%s`" nick);
         match data self nick with
         | Some c ->
           [create_message_for_user now nick c.last_seen]
         | None -> []
       with e ->
         raise (Command.Fail ("seen: " ^ Printexc.to_string e))
    )

let cmd_last (self:t) =
  Command.make_simple_l
    ~descr:"ask for the last n people talking on this chan (default: n=3)"
    ~prio:10 ~cmd:"last"
    (fun _msg s ->
       try
         let default_n = 3 in
         let dest = String.trim s in

         let top_n = try match int_of_string dest with
           | x when x > 0 -> x
           | _ -> default_n
           with
           | Failure _ -> default_n
         in

         Logs.debug ~src:Core.logs_src (fun k->k "query: last `%n`" top_n);

         let now=now() in
         let l = last_talk self ~n:top_n in
         let l = CCList.map (fun (n,t) -> create_message_for_user now n t) l in
         l
       with e ->
         raise (Command.Fail ("last_seen: " ^ Printexc.to_string e))
    )

let cmd_ignore_template ~cmd prefix_stem ignore (self:t) =
  Command.make_simple
    ~descr:(cmd ^ " nick") ~prio:10 ~cmd
    (fun _ s ->
       try
         let dest = String.trim s in
         Logs.debug ~src:Core.logs_src (fun k->k "query: ignore `%s`" dest);
         if String.equal dest "" then (
          None
         ) else (
           let contact = data_or_insert self dest in
           let msg =
             if Bool.equal contact.ignore_user ignore then
               CCFormat.sprintf "already %sing %s" prefix_stem dest |> some
             else (
               set_data self dest
                 { contact with ignore_user = ignore };
               CCFormat.sprintf "%sing %s" prefix_stem dest |> some )
           in
           msg )
       with e ->
         raise (Command.Fail (cmd ^ ": " ^ Printexc.to_string e)))

let cmd_ignore = cmd_ignore_template ~cmd:"ignore" "ignor" true
let cmd_unignore = cmd_ignore_template ~cmd:"unignore" "unignor" false

let cmd_ignore_list (self:t) =
  Command.make_simple_l
    ~descr:"add nick to list of ignored people" ~prio:10 ~cmd:"ignore_list"
    (fun _ _ ->
       try
         Logs.debug ~src:Core.logs_src (fun k->k "query: ignore_list");
         let ignored = ignored self in
         let msg =
           if CCList.is_empty ignored
           then ["no one ignored!"]
           else "ignoring:" :: ignored
         in
         msg
       with e ->
         raise (Command.Fail ("ignore_list: " ^ Printexc.to_string e)))

(* callback to update state, notify users of their messages, etc. *)
let on_message (self:t) (module C:Core.S) msg =
  let module Msg = Irc_message in
  let nick, channel = match msg.Msg.command with
    | Msg.JOIN (_, _) | Msg.PRIVMSG (_, _) ->
      let msg = Core.privmsg_of_msg msg |> unwrap_opt "message parsing error" in
      Some msg.nick, if Core.is_chan msg.to_ then Some msg.to_ else None
    | Msg.NICK newnick ->
      Some newnick, None
    | _ -> None, None
  in
  (* trigger [tell] messages *)
  begin match nick with
    | None -> ()
    | Some nick ->
      (* update [lastSeen] *)
      let now = now() in
      let contact =
        update_data self nick
          ~f:(fun st -> {st with last_seen = now })
      in
      let to_tell, remaining =
        contact.to_tell
        |> List.partition
          (fun t -> match t.tell_after, channel with
             | _, None -> false
             | None, _ -> true
             | Some f, Some chan ->
               (* delay expired, and it was on the same channel *)
               Float.(now > f) && String.equal t.on_channel chan
          )
      in
      if not (List.is_empty to_tell) then (
        set_data self nick {contact with to_tell = remaining};
      );
      List.iter (fun {from=author; on_channel; msg=m; _} ->
        C.send_notice ~target:on_channel
          ~message:(Printf.sprintf "%s: (from %s): %s" nick author m))
        (List.rev to_tell)
  end

let plugin =
  let commands self =
    [ cmd_tell self;
      cmd_tell_at self;
      cmd_seen self;
      cmd_last self;
      cmd_ignore self;
      cmd_unignore self;
      cmd_ignore_list self;
    ]
  in
  Plugin.db_backed
    ~commands ~prepare_db
    ~on_msg:(fun st -> [on_message st])
    ()
