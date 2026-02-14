(** {1 Core IRC state} *)

module Format_ = Format
open Prelude
module Msg = Irky.Message

let logs_src = Logs.Src.create ~doc:"logs for calculon" "calculon"

module Log = (val Logs.src_log logs_src)

type irc_msg = Irky.Message.t

type privmsg = {
  nick: string; (* author *)
  to_: string; (* target *)
  message: string;
}

let is_chan s =
  (not (String.equal s ""))
  && Char.equal s.[0] '#'
  && not (String.contains s ' ')

let nick msg = msg.nick

let reply_to msg =
  if is_chan msg.to_ then
    msg.to_
  (* reply on same channel *)
  else
    msg.nick
(* in pv *)

let privmsg_of_msg msg =
  match msg.Msg.command with
  | Msg.PRIVMSG (to_, message) ->
    Some
      {
        nick = Option.get_or "msg prefix" msg.Msg.prefix |> get_nick;
        to_;
        message;
      }
  | _ -> None

let string_of_privmsg msg =
  Printf.sprintf "{nick:%S; to:%S; msg: %S}" msg.nick msg.to_ msg.message

type t = {
  mutable client: Irky.Client.t option;
  mutable line_cut_threshold: int;
  io: Irky.Io.t;
}
(** The core bot state, wrapping an irky client connection. *)

let line_cut_threshold t = t.line_cut_threshold
let set_line_cut_threshold t n = t.line_cut_threshold <- n

let client_exn (self : t) =
  match self.client with
  | Some c -> c
  | None -> failwith "not connected"

let process_list_ ?(bypass_limit = false) ?sep ~f ~target ~messages:lines
    (self : t) =
  let client = client_exn self in
  (* keep at most 4, unless [bypass_limit=true] *)
  let lines =
    let len = List.length lines in
    if (not bypass_limit) && len > self.line_cut_threshold then
      CCList.take 4 lines @ [ Printf.sprintf "(…%d more lines…)" (len - 4) ]
    else
      lines
  in
  let delay_between = ref 0.3 in
  List.iter
    (fun message ->
      f client ~target ~message;
      self.io.sleep !delay_between;
      delay_between := CCFloat.min (!delay_between +. 0.2) 1.0;
      match sep with
      | None -> ()
      | Some f -> f ())
    lines

let split_lines_ = CCString.Split.list_cpy ~by:"\n"
let flat_map = CCList.flat_map

let send_privmsg_l (self : t) ~target ~messages =
  process_list_ ~f:Irky.Client.send_privmsg ~target
    ~messages:(flat_map split_lines_ messages)
    self

let send_privmsg_l_nolimit ?(delay = 0.5) (self : t) ~target ~messages () =
  process_list_ ~f:Irky.Client.send_privmsg
    ~sep:(fun () -> self.io.sleep delay)
    ~target ~bypass_limit:true
    ~messages:(flat_map split_lines_ messages)
    self

let send_notice_l (self : t) ~target ~messages =
  process_list_ ~f:Irky.Client.send_notice ~target ~bypass_limit:false
    ~messages:(flat_map split_lines_ messages)
    self

let send_privmsg (self : t) ~target ~message =
  process_list_ ~target ~messages:(split_lines_ message)
    ~f:Irky.Client.send_privmsg self

let send_notice (self : t) ~target ~message =
  process_list_ ~target ~messages:(split_lines_ message)
    ~f:Irky.Client.send_notice self

let send_join (self : t) ~channel =
  Irky.Client.send_join (client_exn self) ~channel

let send_part (self : t) ~channel =
  Irky.Client.send_part (client_exn self) ~channels:[ channel ]
    ~message:"bye y'all"

let talk (self : t) ~target ty =
  let message = Talk.select ty in
  send_privmsg self ~target ~message

let make_io_tls ~check_cert ~net ~clock ~sw : Irky.Io.t =
  let config = Irky_eio.Ssl_config.make ~check_certificate:check_cert () in
  Irky_eio.io_ssl ~config ~net ~clock ~sw

let make_io_plain ~net ~clock ~sw : Irky.Io.t = Irky_eio.io ~net ~clock ~sw

let run ~sw ~(net : Eio_unix.Net.t) ~(clock : _ Eio.Time.clock) conf ~on_connect
    ~callback () =
  let module C = Config in
  let io =
    if conf.C.tls then
      make_io_tls ~check_cert:conf.C.check_cert ~net ~clock ~sw
    else
      make_io_plain ~net ~clock ~sw
  in
  let connect () =
    Irky.Client.connect ~username:conf.C.username ~realname:conf.C.realname
      ~nick:conf.C.nick ?password:conf.C.password ~server:conf.C.server
      ~port:conf.C.port ~sasl:conf.C.sasl ~io ()
  in
  let core = { client = None; line_cut_threshold = 10; io } in
  Irky.Client.reconnect_loop ~reconnect_delay:60. ~io ~connect
    ~on_connect:(fun client ->
      let conn_info = Printf.sprintf "%s/%d" conf.C.server conf.C.port in
      Log.info (fun k -> k "connected to %s" conn_info);
      core.client <- Some client;
      on_connect core)
    (fun _client msg ->
      try callback core msg
      with exn ->
        Log.err (fun k ->
            k "error in message callback: %s" (Printexc.to_string exn)))
