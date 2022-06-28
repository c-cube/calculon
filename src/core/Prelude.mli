(** {1 helpers} *)

val string_opt_to_string : string option -> string

val string_list_to_string : string list -> string

val get_nick : string -> string

val (|?) : 'a option -> 'a -> 'a
(** [o |? x] is [y] if [o=Some y], [x] otherwise *)

val contains : string -> Re.re -> bool

val re_match2 : (string -> string -> 'a) -> Re.re -> string -> 'a option

val re_match1 : (string -> 'a) -> Re.re -> string -> 'a option

val re_match0 : 'a -> Re.re -> string -> 'a option

val edit_distance : string -> string -> int

module StrMap : CCMap.S with type key = string

(** {2 Random Distribution} *)
module Rand_distrib : sig
  type 'a t = ('a * float) list

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val add : 'a -> float -> 'a t -> 'a t
  val binjoin : 'a t -> 'a t -> 'a t
  val join : 'a t list -> 'a t

  val uniform : 'a list -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val top : 'a t -> 'a t
  val bot : 'a t -> 'a t
  val normalize : 'a t -> 'a t

  val run : 'a t -> 'a
  (** Pick a value in the given distribution *)
end

val random_l : 'a list -> 'a
(** Random choice in list, shortcut for {!Rand_distrib} *)
