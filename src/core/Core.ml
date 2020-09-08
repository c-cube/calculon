(** {1 Core IRC state} *)

module Format_ = Format

open Prelude
open Containers

open Lwt.Infix

module Msg = Irc_message
type irc_msg = Irc_message.t
let logs_src = Logs.Src.create ~doc:"logs for calculon" "calculon"

type privmsg = {
  nick: string; (* author *)
  to_: string; (* target *)
  message: string;
}

let is_chan s =
  not (String.equal s "") && Char.equal s.[0] '#' && not (String.contains s ' ')

let nick msg = msg.nick

let reply_to msg =
  if is_chan msg.to_
  then msg.to_ (* reply on same channel *)
  else msg.nick (* in pv *)

let privmsg_of_msg msg =
  match msg.Msg.command with
  | Msg.PRIVMSG (to_, message) ->
    Some
      { nick = Option.get_exn msg.Msg.prefix |> get_nick;
        to_;
        message }
  | _ -> None

let string_of_privmsg msg =
  Printf.sprintf "{nick:%s, to:%s, msg: %s}" msg.nick msg.to_ msg.message

module type S = sig
  module I : Irc_client.CLIENT with type 'a Io.t = 'a Lwt.t

  type connection = I.connection_t

  val connection : connection

  val init : unit Lwt.t
  val exit : unit Lwt.t

  val log : string -> unit Lwt.t
  [@@ocaml.deprecated "use library logs instead"]

  val logf : ('a, Format.t, unit, unit Lwt.t) format4 -> 'a
  [@@ocaml.deprecated "use library logs instead"]

  val set_log : (string -> unit Lwt.t) -> unit
  [@@ocaml.deprecated "use library logs instead"]

  val send_exit : unit -> unit

  val messages : Msg.t Signal.t

  val privmsg : privmsg Signal.t

  val line_cut_threshold : int ref
  (** Above [!line_cut_threshold], multi-line messages are cut with "..." *)

  val send_privmsg_l :
    target:string -> messages:string list -> unit Lwt.t

  val send_privmsg_l_nolimit :
    ?delay:float ->
    target:string ->
    messages:string list ->
    unit ->
    unit Lwt.t
  (** Version of {!send_privmsg_l} that does not enforce cut threshold.
      @param delay optional delay between each sent message *)

  val send_privmsg :
    target:string -> message:string -> unit Lwt.t
  (** Helper for sending messages, splitting lines, etc. *)

  val send_notice_l :
    target:string -> messages:string list -> unit Lwt.t

  val send_notice :
    target:string -> message:string -> unit Lwt.t
  (** Helper for sending notices, splitting lines, etc. *)

  val send_join : channel:string -> unit Lwt.t

  val send_part : channel:string -> unit Lwt.t

  val talk : target:string -> Talk.t -> unit Lwt.t
end

type t = (module S)

module Make
    (I : Irc_client.CLIENT with type 'a Io.t = 'a Lwt.t)
    (Conn : sig val c : I.connection_t end)
= struct
  module I = I

  type connection = I.connection_t
  let connection = Conn.c

  let init = Lwt.return_unit (* already done! *)
  let exit, send_exit = Lwt.wait ()

  let send_exit () = Lwt.wakeup send_exit ()

  let set_log _ =
    Logs.err ~src:logs_src (fun k->k "call to deprecated `set_log` function");
    ()

  let log s =
    Logs.err ~src:logs_src (fun k->k "call to deprecated `log` function");
    Logs.debug ~src:logs_src (fun k->k "log(): %s" s);
    Lwt.return_unit

  let logf msg = Format.kasprintf log msg

  let messages = Signal.create ()
  let privmsg = Signal.filter_map messages privmsg_of_msg

  let line_cut_threshold = ref 10

  let process_list_ ?(bypass_limit=false) ?sep ~f ~target ~messages:lines () =
    (* keep at most 4 *)
    let lines =
      let len = List.length lines in
      if not bypass_limit && len > !line_cut_threshold
      then CCList.take 4 lines @ [Printf.sprintf "(…%d more lines…)" (len-4)]
      else lines
    in
    Lwt_list.iter_s
      (fun message ->
         f ~connection ~target ~message >>= fun () ->
         match sep with
           | None -> Lwt.return_unit
           | Some f -> f())
      lines

  let split_lines_ = CCString.Split.list_cpy ~by:"\n"

  let flat_map f l = List.map f l |> List.flatten

  let send_privmsg_l ~target ~messages =
    process_list_
      ~f:I.send_privmsg ~target
      ~messages:(flat_map split_lines_ messages) ()

  let send_privmsg_l_nolimit ?(delay=0.5) ~target ~messages () =
    process_list_
      ~f:I.send_privmsg
      ~sep:(fun () -> Lwt_unix.sleep delay)
      ~target  ~bypass_limit:true
      ~messages:(flat_map split_lines_ messages)
      ()

  let send_notice_l ~target ~messages =
    process_list_
      ~f:I.send_notice ~target  ~bypass_limit:false
      ~messages:(flat_map split_lines_ messages) ()

  let send_privmsg ~target ~message =
    process_list_
      ~target ~messages:(split_lines_ message) ~f:I.send_privmsg ()

  let send_notice ~target ~message =
    process_list_
      ~target ~messages:(split_lines_ message) ~f:I.send_notice ()

  let send_join ~channel =
    I.send_join ~connection ~channel

  let send_part ~channel =
    I.send ~connection Irc_message.({prefix=None; command=PART ([channel], "bye y'all")})

  let talk ~target ty =
    let message = Talk.select ty in
    send_privmsg ~target ~message

  let () =
    let src = Logs.Src.create "irc-client" ~doc:"ocaml-irc-client" in
    I.set_log (fun s -> Logs.debug ~src (fun k -> k "%s" s); Lwt.return_unit)
end

module Run
    (I : Irc_client.CLIENT with type 'a Io.t = 'a Lwt.t)
    (F : sig
       val connect: unit -> I.connection_t option Lwt.t
       val init: t -> unit Lwt.t
     end)
= struct
  let run () : unit Lwt.t =
    let self : t option ref = ref None in
    I.reconnect_loop
      ~keepalive:{I.mode=`Passive; timeout=300}
      ~after:60
      ~connect:(fun () ->
          Logs.info ~src:logs_src (fun k->k "trying to (re)connect…");
          F.connect ())
      ~callback:(fun _ msg_or_err ->
        match !self with
        | None -> Lwt.return_unit
        | Some (module C) ->
          begin match msg_or_err with
            | Result.Ok msg ->
              Logs.debug ~src:logs_src
                (fun k->k "got message %s" (Irc_message.to_string msg));
              Signal.send C.messages msg
            | Result.Error err ->
              Logs.err ~src:logs_src (fun k->k "error: %s" err);
              Lwt.return ()
          end)
      ~f:(fun conn ->
        Logs.info ~src:logs_src (fun k->k "connected, instantiate core");
        let module C = Make(I)(struct let c = conn end) in
        let new_c = (module C : S) in
        self := Some new_c;
        F.init new_c)
      ()
end

let loop_ssl ~connect ~init () : unit Lwt.t =
  let module R = Run(Irc_client_lwt_ssl)(struct
      let connect = connect
      let init = init
    end) in
  R.run ()

let loop_unsafe ~connect ~init () : unit Lwt.t =
  let module R = Run(Irc_client_lwt)(struct
      let connect = connect
      let init = init
    end) in
  R.run ()

let run conf ~init () =
  let module C = Config in
  let init (core:t) =
    let (module C) = core in
    init core
  in
  if conf.C.tls then (
    let tls_config = Irc_client_lwt_ssl.Config.default in
    let connect () =
      Irc_client_lwt_ssl.connect_by_name
        ~username:conf.C.username ~realname:conf.C.realname ~nick:conf.C.nick
        ~server:conf.C.server ~port:conf.C.port ~config:tls_config
        ()
    in
    loop_ssl ~connect ~init ()
  ) else (
    let connect () =
      Irc_client_lwt.connect_by_name
        ~username:conf.C.username ~realname:conf.C.realname ~nick:conf.C.nick
        ~server:conf.C.server ~port:conf.C.port
        ()
    in
    loop_unsafe ~connect ~init ()
  )
