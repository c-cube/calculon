(** User-defined config *)

type hidden (** Type that cannot be built *)

type t = {
  server : string; (** Address of the irc server *)
  port : int; (** Port of the server *)
  username : string;
  password : string option;
  realname : string;
  nick : string;
  tls: bool;
  sasl: bool;
  channels : string list; (** Channels to join after the connexion to the server *)

  log_level: Logs.level;
  (** Level of logging.
      @since 0.6 *)

  prefix: string; (** prefix for commands *)

  db_file: string;
  (** Database path. @since 0.8 *)

  _hidden: hidden;
  (** This field is present to prevent the user from using a literal
      record to build configuration. This way, adding new fields
      doesn't break existing code.

      @since 0.8 *)
}
(** Bot configuration. *)

val default : t
(** Default configuration:
- server = "irc.libera.chat"
- port = 7000
- username = "calculon"
- realname = "calculon"
- password = None
- nick = "calculon"
- tls = true
- sasl = true
- channel = "#ocaml"
- irc_log = `None
- log_level = Logs.Warning
- prefix = "!"
- db_file = "calculon.db"
 *)

val parse :
  ?extra_args:(string * Arg.spec * string) list ->
  t -> string array -> t
(** [parse conf args] is the same as [conf], but some command line
    arguments can override its fields
    @param extra_args additional command line arguments for {!Arg} (since 0.8)
*)

val of_argv :
  ?extra_args:(string * Arg.spec * string) list ->
  unit -> t
(** Parsed from {!Sys.argv}
    Will call {!exit} if [Arg.parse] fails
    @param extra_args additional command line arguments for {!Arg} (since 0.8)
*)
