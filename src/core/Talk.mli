
type t =
  | Ack
  | Err

val select : t -> string
