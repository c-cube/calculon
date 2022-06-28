let spf = Printf.sprintf
let[@inline] ( let@ ) f x = f x
let id x = x

module Option = struct
  include Option

  let get_or msg = function
    | Some x -> x
    | None -> failwith msg

  let get_or_lazy default = function
    | Some x -> x
    | None -> default ()

  module Infix = struct
    let ( let+ ) x f = Option.map f x
    let ( let* ) = Option.bind
    let ( >|= ) x f = Option.map f x
    let ( >>= ) = Option.bind

    let ( and+ ) x y =
      match x, y with
      | None, _ | _, None -> None
      | Some x, Some y -> Some (x, y)
  end
end

let unwrap_result_failwith = function
  | Ok x -> x
  | Error msg -> failwith msg

let wrap_failwith ctx f =
  try f ()
  with exn ->
    let err =
      match exn with
      | Failure e -> spf "%s\n%s" e ctx
      | e -> spf "%s\n%s" (Printexc.to_string e) ctx
    in
    Logs.err (fun k -> k "fail: %s" err);
    failwith err

let guard_res ?(ctx = "") f : _ result =
  try Ok (f ()) with
  | Failure e -> Error e
  | e -> Error (ctx ^ Printexc.to_string e)

module Lwt_infix = struct
  let ( let* ) = Lwt.bind
  let ( let+ ) x f = Lwt.map f x
  let ( and+ ) = Lwt.both
  let ( and* ) = ( and+ )
  let ( >|= ) x f = Lwt.map f x
  let ( >>= ) = Lwt.bind
end
