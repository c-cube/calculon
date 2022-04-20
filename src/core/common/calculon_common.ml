
let spf = Printf.sprintf
let[@inline] (let@) f x = f x

let id x = x
let some x = Some x
let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let unwrap_opt msg = function
  | Some x -> x
  | None -> failwith msg

let unwrap_result_failwith = function
  | Ok x -> x
  | Error msg -> failwith msg

let wrap_failwith ctx f =
  try f()
  with
  | exn ->
    let err = match exn with
      | Failure e -> spf "%s\n%s" e ctx
      | e -> spf "%s\n%s" (Printexc.to_string e) ctx
    in
    Logs.err (fun k->k "fail: %s" err);
    failwith err

let guard_res ?(ctx="") f : _ result =
  try Ok (f())
  with
  | Failure e -> Error e
  | e -> Error (ctx^Printexc.to_string e)

let spawn_thread f x =
  let run() =
    List.iter
      (fun i -> Sys.set_signal i Sys.Signal_ignore)
      [Sys.sigint; Sys.sigterm; Sys.sigpipe];
    f x
  in
  ignore (Thread.create run () : Thread.t)
