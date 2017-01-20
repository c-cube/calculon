(** {1 helpers} *)

let string_opt_to_string = function
  | None -> "None"
  | Some s -> Printf.sprintf "Some %s" s

let string_list_to_string string_list =
  Printf.sprintf "[%s]" (String.concat "; " string_list)

let get_nick h =
  Str.split_delim (Str.regexp "!") h |> List.hd

let id x = x
let some x = Some x
let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let (|?) o x = match o with
  | None -> x
  | Some y -> y

let contains s x =
  try Str.search_forward x s 0 |> ignore; true
  with Not_found -> false

let re_match2 f r s =
  if Str.string_match r s 0
  then f (Str.matched_group 1 s) (Str.matched_group 2 s) |> some
  else None

let re_match1 f r s =
  if Str.string_match r s 0
  then f (Str.matched_group 1 s) |> some
  else None

let re_match0 x r s =
  if Str.string_match r s 0
  then Some x
  else None

let select l = DistribM.run @@ DistribM.uniform l

module StrMap = CCMap.Make(String)

include Lwt.Infix
