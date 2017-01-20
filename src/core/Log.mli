(** {1 Simple logging} *)

val log : string -> unit

val logf : ('a, unit, string, unit) format4 -> 'a
