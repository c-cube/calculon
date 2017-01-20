(** {1 helpers} *)

val string_opt_to_string : string option -> string

val string_list_to_string : string list -> string

val get_nick : string -> string

val id : 'a -> 'a

val some : 'a -> 'a option

val map_opt : ('a -> 'b) -> 'a option -> 'b option

val (|?) : 'a option -> 'a -> 'a
(** [o |? x] is [y] if [o=Some y], [x] otherwise *)

val contains : string -> Str.regexp -> bool

val re_match2 : (string -> string -> 'a) -> Str.regexp -> string -> 'a option

val re_match1 : (string -> 'a) -> Str.regexp -> string -> 'a option

val re_match0 : 'a -> Str.regexp -> string -> 'a option

val select : 'a list -> 'a
(** Random choice *)

module StrMap : CCMap.S with type key = string


