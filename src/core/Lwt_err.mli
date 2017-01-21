
(** {1 Lwt+Result Monad} *)

type 'a t = ('a, string) Result.result Lwt.t

val return : 'a -> ('a, 'b) result Lwt.t

val fail : 'a -> ('b, 'a) result Lwt.t

val lift : ('a, string) Result.result -> 'a t

val ok : 'a Lwt.t -> 'a t

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

val ( >>?= ) : 'a t -> ('a -> ('b, string) Result.result) -> 'b t

val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

val map_s : ('a -> 'b t) -> 'a list -> 'b list t

val map_err : (string -> string) -> 'a t -> 'a t

