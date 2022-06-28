
(** Lock protecting a value *)

type 'a t

val create : 'a -> 'a t

val with_ : 'a t -> ('a -> 'b) -> 'b
