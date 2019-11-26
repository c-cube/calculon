(** User-defined config *)

type irc_log =
  [ `None
  | `Chan of Lwt_io.output_channel
  | `Custom of (string -> unit Lwt.t)
  ]

type t = {
  server : string; (** Address of the irc server *)
  port : int; (** Port of the server *)
  username : string;
  realname : string;
  nick : string;
  tls: bool;
  tls_cert : Ssl.certificate option;
  channel : string; (** Channel to join after the connexion to the server *)
  state_file : string; (** Where plugins' state is stored *)
  irc_log: irc_log; (** Log IRC events *)
  prefix: string; (** prefix for commands *)
}

val default : t
(** Default configuration:
- server = "irc.freenode.net"
- port = 7000
- username = "calculon"
- realname = "calculon"
- nick = "calculon"
- tls = true
- tls_cert = None
- channel = "#ocaml"
- state_file = "state.json"
- irc_log = `None
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
