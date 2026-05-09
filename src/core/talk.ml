(* À étendre *)

type t = Ack | Err

let ack = [ "OK."; "done."; "Success" ]
let error = [ "oops"; "uh"; "hmm"; "Failure" ]

let talk_base = function
  | Ack -> ack
  | Err -> error

let select ty = talk_base ty |> Prelude.random_l
