(** {1 Simple logging} *)

let verbose = ref false

let log msg =
  if !verbose then Printf.printf "Log: %s\n%!" msg

let logf msg = Printf.ksprintf log msg
