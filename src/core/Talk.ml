
(* À étendre *)

type t =
  | Ack
  | Err

let ack = [
  "Bien reçu";
  "OK.";
  "done.";
]

let error = [
  "oops";
  "nop";
  "coucOUPS";
]

let talk_base = function
  | Ack -> ack
  | Err -> error

let select ty = talk_base ty |> Prelude.select
