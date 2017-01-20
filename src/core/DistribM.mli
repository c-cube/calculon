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
val run : 'a t -> 'a
val normalize : 'a t -> 'a t
