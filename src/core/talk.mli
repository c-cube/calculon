(** {1 Basic Messages}

    These messages are typical replies to user-issued commands, with a bit of
    randomness to diminish the monotony *)

type t = Ack  (** Ok *) | Err  (** Error occurred *)

val select : t -> string
(** A particular representation of this message *)
