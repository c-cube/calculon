(** {1 helpers} *)

let string_opt_to_string = function
  | None -> "None"
  | Some s -> Printf.sprintf "Some %s" s

let string_list_to_string string_list =
  Printf.sprintf "[%s]" (String.concat "; " string_list)

let get_nick h = CCString.Split.left_exn ~by:"!" h |> fst

let id x = x
let some x = Some x
let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let (|?) o x = match o with
  | None -> x
  | Some y -> y

let contains s (re:Re.re) = Re.execp re s

let re_match2 f r s = match Re.exec_opt r s with
  | None -> None
  | Some g ->
    f (Re.Group.get g 1) (Re.Group.get g 2) |> some

let re_match1 f r s = match Re.exec_opt r s with
  | None -> None
  | Some g -> f (Re.Group.get g 1) |> some

let re_match0 x r s =
  if contains s r then Some x else None

(* from containers 1.0 *)
let edit_distance s1 s2 =
  if String.length s1 = 0
  then String.length s2
  else if String.length s2 = 0
  then String.length s1
  else if s1 = s2
  then 0
  else begin
    (* distance vectors (v0=previous, v1=current) *)
    let v0 = Array.make (String.length s2 + 1) 0 in
    let v1 = Array.make (String.length s2 + 1) 0 in
    (* initialize v0: v0(i) = A(0)(i) = delete i chars from t *)
    for i = 0 to String.length s2 do
      v0.(i) <- i
    done;
    (* main loop for the bottom up dynamic algorithm *)
    for i = 0 to String.length s1 - 1 do
      (* first edit distance is the deletion of i+1 elements from s *)
      v1.(0) <- i+1;

      (* try add/delete/replace operations *)
      for j = 0 to String.length s2 - 1 do
        let cost = if Char.compare (String.get s1 i) (String.get s2 j) = 0 then 0 else 1 in
        v1.(j+1) <- min (v1.(j) + 1) (min (v0.(j+1) + 1) (v0.(j) + cost));
      done;

      (* copy v1 into v0 for next iteration *)
      Array.blit v1 0 v0 0 (String.length s2 + 1);
    done;
    v1.(String.length s2)
  end

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
