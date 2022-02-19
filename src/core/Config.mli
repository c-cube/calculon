(** User-defined config *)

type t = {
  server : string; (** Address of the irc server *)
  port : int; (** Port of the server *)
  username : string;
  password : string option;
  realname : string;
  nick : string;
  tls: bool;
  sasl: bool;
  channel : string; (** Channel to join after the connexion to the server *)
  state_file : string; (** Where plugins' state is stored *)

  log_level: Logs.level;
  (** Level of logging.
      @since 0.6 *)

  prefix: string; (** prefix for commands *)
}

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
- state_file = "state.json"
- irc_log = `None
- log_level = Logs.Warning
- prefix = "!"
 *)

val parse :
  ?extra_args:(string * Arg.spec * string) list ->
  t -> string array -> t
(** [parse conf args] is the same as [conf], but some command line
    arguments can override its fields
    @param extra_args additional command line arguments for {!Arg} (since NEXT_RELEASE)
*)

val of_argv :
  ?extra_args:(string * Arg.spec * string) list ->
  unit -> t
(** Parsed from {!Sys.argv}
    Will call {!exit} if [Arg.parse] fails
    @param extra_args additional command line arguments for {!Arg} (since NEXT_RELEASE)
*)
