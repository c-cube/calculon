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
