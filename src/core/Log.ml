(** {1 Simple logging} *)

let log msg = Printf.printf "Log: %s\n%!" msg

let logf msg = Printf.ksprintf log msg
