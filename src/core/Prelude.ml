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

module StrMap = CCMap.Make(String)

include Lwt.Infix

(** {2 Random Distribution} *)
module Rand_distrib = struct
  type 'a t = ('a * float) list

  let return x = [x, 1.]

  let rec add x p = function
    | [] -> [x, p]
    | (y, q) :: t ->
      if x = y then (y, q +. p) :: t else (y, q) :: (add x p t)

  let rec (>>=) (a : 'a t) (b : 'a -> 'b t) : 'b t =
    match a with
      | [] -> []
      | (x, t) :: tl ->
        List.fold_left
          (fun pre (c, u) -> add c (u *. t) pre)
          (tl >>= b)
          (b x)

  let binjoin a b = List.map (fun (x, d) -> x, d /. 2.) (a @ b)

  let join l =
    let flatten = List.fold_left (@) [] in
    let n = List.length l in
    flatten (List.map (List.map (fun (x, d) -> x, d /. (float_of_int n))) l)

  let uniform l = join (List.map return l)

  let filter p l = List.filter (fun (a, _) -> p a) l

  let top d =
    let m = List.fold_left (fun b (_, u) -> max b u) 0. d in
    List.filter (fun (_, u) -> u = m) d

  let bot d =
    let m = List.fold_left (fun b (_, u) -> min b u) 2. d in
    List.filter (fun (_, u) -> u = m) d

  let () = Random.self_init ()

  let run x =
    let rec aux f = function
      | [] -> assert false
      | [ v, _ ] -> v
      | ((v, h) :: t) -> if f <= h then v else aux (f -. h) t in
    aux (Random.float 1.) x

  let normalize l =
    let i = List.fold_left (fun a (_, b) -> a +. b) 0. l in
    List.map (fun (a, k) -> a, k /. i) l
end

let random_l l = Rand_distrib.(run @@ uniform l)
