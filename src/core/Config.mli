(* User-defined config *)

type t = {
  server : string;
  port : int;
  username : string;
  realname : string;
  nick : string;
  tls: bool;
  channel : string;
  state_file : string; (* where plugins' state is stored *)
}

val default : t

val parse : t -> string array -> t
(** [parse conf args] is the same as [conf], but some command line
    arguments can override its fields *)

val of_argv : unit -> t
(** Parsed from {!Sys.argv}
    Will call {!exit} if [Arg.parse] fails *)
