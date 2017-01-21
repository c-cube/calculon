
(* À étendre *)

type t =
  | Ack
  | Err

let ack = [
  "OK.";
  "done.";
]

let error = [
  "oops";
]

let talk_base = function
  | Ack -> ack
  | Err -> error

let select ty = talk_base ty |> Prelude.select
