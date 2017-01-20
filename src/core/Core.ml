
(** {1 Core IRC state} *)

open Prelude
open Containers

open Lwt.Infix

module Msg = Irc_message
module Irc = Irc_client_tls

type connection = Irc.connection_t

type privmsg = {
  nick: string; (* author *)
  to_: string; (* target *)
  message: string;
}

let is_chan s =
  s<>"" && s.[0] = '#' && not (String.contains s ' ')

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
  val connection : connection

  val init : unit Lwt.t
  val exit : unit Lwt.t

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

  val talk : target:string -> Talk.t -> unit Lwt.t
end

type t = (module S)

let of_conn (c:connection): t =
  let module M = struct
    let connection = c

    let init = Lwt.return_unit (* already done! *)
    let exit, send_exit = Lwt.wait ()

    let send_exit () = Lwt.wakeup send_exit ()

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

    let split_lines_ s =
      let nl = Str.regexp_string "\n" in
      Str.split nl s

    let flat_map f l = List.map f l |> List.flatten

    let send_privmsg_l ~target ~messages =
      process_list_
        ~f:Irc.send_privmsg ~target
        ~messages:(flat_map split_lines_ messages) ()

    let send_privmsg_l_nolimit ?(delay=0.5) ~target ~messages () =
      process_list_
        ~f:Irc.send_privmsg
        ~sep:(fun () -> Lwt_unix.sleep delay)
        ~target  ~bypass_limit:true
        ~messages:(flat_map split_lines_ messages)
        ()

    let send_notice_l ~target ~messages =
      process_list_
        ~f:Irc.send_notice ~target  ~bypass_limit:false
        ~messages:(flat_map split_lines_ messages) ()

    let send_privmsg ~target ~message =
      process_list_
        ~target ~messages:(split_lines_ message) ~f:Irc.send_privmsg ()

    let send_notice ~target ~message =
      process_list_
        ~target ~messages:(split_lines_ message) ~f:Irc.send_notice ()

    let send_join ~channel =
      Irc.send_join ~connection ~channel

    let talk ~target ty =
      let message = Talk.select ty in
      send_privmsg ~target ~message
  end in
  (module M : S)

let run ~connect ~init () : unit Lwt.t =
  let self : t option ref = ref None in
  Irc.reconnect_loop
    ~keepalive:{Irc. mode=`Passive; timeout=300}
    ~after:60
    ~connect
    ~callback:(fun _ msg_or_err -> match !self with
      | None -> Lwt.return_unit
      | Some (module C) ->
        begin match msg_or_err with
          | Result.Ok msg -> Signal.send C.messages msg
          | Result.Error err ->
            Printf.eprintf "%s\n%!" err;
            Lwt.return ()
        end)
    ~f:(fun conn ->
      let new_c = of_conn conn in
      self := Some new_c;
      init new_c)
    ()

let connect_of_config conf =
  let module C = Config in
  let connect () =
    Irc.connect_by_name
      ~username:conf.C.username ~realname:conf.C.realname ~nick:conf.C.nick
      ~server:conf.C.server ~port:conf.C.port
      ()
  in
  connect

let () =
  Irc.set_log (fun s -> Log.log s; Lwt.return_unit)
