
(** {1 Core IRC state} *)

type irc_msg = Irc_message.t

(** A message sent on some IRC channel, or in query.
    The message content is [message], its sender is [nick] (a nickname),
    and its target is [to_] (either a nickname for private messages,
    or a channel name).
*)
type privmsg = {
  nick: string; (** author: nick *)
  to_: string; (** target: nick or chan *)
  message: string; (** actual content *)
}

val is_chan : string -> bool
(** Is this a valid chan name? *)

val reply_to : privmsg -> string
(** find whom to reply to. If the message is in query,
    that is, [not (is_chan msg.to_)], then reply to [msg.nick];
    otherwise reply on the channel [msg.to_].
*)

val nick : privmsg -> string
(** The author of the message *)

val privmsg_of_msg : irc_msg -> privmsg option
(** Try to parse a {!privmsg} from a raw IRC message *)

val string_of_privmsg : privmsg -> string

module type S = sig
  module I : Irc_client.CLIENT with type 'a Io.t = 'a Lwt.t

  type connection = I.connection_t

  val connection : connection
  (** Current connection. *)

  val init : unit Lwt.t
  val exit : unit Lwt.t

  val log : string -> unit Lwt.t
  (** Log a message (see field [irc_log] of {!Config.config}),
      typically for debugging purpose.
      @since 0.5 *)

  val logf : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  (** Pretty logger (calling {!log} to print the formatted message).
      Example: [logf "answer to %s is %d" "life" 42]
      @since 0.5 *)

  val set_log : (string -> unit Lwt.t) -> unit
  (** Set logger (default is the empty logger) that will be used by {!log}
      @since 0.5 *)

  val send_exit : unit -> unit
  (** trigger the {!exit} signal (only at the end!) *)

  val messages : irc_msg Signal.t
  (** Signal triggered every time any IRC message is received *)

  val privmsg : privmsg Signal.t
  (** Signal triggered every time a {!privmsg} is received *)

  val line_cut_threshold : int ref
  (** Above [!line_cut_threshold], multi-line messages are cut with "..." *)

  val send_privmsg_l :
    target:string -> messages:string list -> unit Lwt.t
  (** Send a list of messages to the target. If the list
      is too long (see {!line_cut_threshold}) only a prefix of the list
      will be sent *)

  val send_privmsg_l_nolimit :
    ?delay:float ->
    target:string ->
    messages:string list ->
    unit ->
    unit Lwt.t
  (** Version of {!send_privmsg_l} that does not enforce cut threshold.
      Be careful of the flood this might cause.
      @param delay optional delay between each sent message *)

  val send_privmsg :
    target:string -> message:string -> unit Lwt.t
  (** Helper for sending messages, splitting lines, etc. *)

  val send_notice_l :
    target:string -> messages:string list -> unit Lwt.t
  (** Send a list of notices. Notices are not supposed to be
      parsed by other bots, so as to avoid reply loops *)

  val send_notice :
    target:string -> message:string -> unit Lwt.t
  (** Helper for sending notices, splitting lines, etc. *)

  val send_join : channel:string -> unit Lwt.t
  (** Send a "join" messages to try and join some channel *)

  val send_part : channel:string -> unit Lwt.t
  (** Send a "part" messages to try and part some channel *)

  val talk : target:string -> Talk.t -> unit Lwt.t
  (** Send a pre-formatted answer to the channel. *)
end

type t = (module S)

val loop_ssl :
  connect:(unit -> Irc_client_lwt_ssl.connection_t option Lwt.t) ->
  init:(t -> unit Lwt.t) ->
  unit ->
  unit Lwt.t
(** Feed this to {!Lwt_main.run}. It will connect using
    [connect] (which opens a TLS connection),
    create a new core object (of type {!t}),
    call [init] on this core object (to initialize plugins, etc.)
    then loop on incoming messages.
    If the connection is closed for any reason, this will wait
    for some time and then re-connect and call {!init} again, etc. *)

val loop_unsafe :
  connect:(unit -> Irc_client_lwt.connection_t option Lwt.t) ->
  init:(t -> unit Lwt.t) ->
  unit ->
  unit Lwt.t
(** Feed to {!Lwt_main.run}. Same as {!loop_tls} but with a cleartext
    connection (boo!). *)

val run :
  Config.t ->
  init:(t -> unit Lwt.t) ->
  unit ->
  unit Lwt.t
(** Main entry point: use config to pick the connection method,
    then call the appropriate auto-reconnection loop.
    Calls {!init} every time a new connection is opened. *)

(** Logging *)

val logs_src: Logs.Src.t
(** Logs from Calculon should use this source.
    @since NEXT_RELEASE *)
