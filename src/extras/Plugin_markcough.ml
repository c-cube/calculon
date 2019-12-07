
(* This file is free software. See file "license" for more details. *)

(** {1 Markov Chains} *)

open Calculon

(** {2 Transition Table} *)
module Table = struct
  type token = string (* "" is start; non empty string otherwie *)

  let print_token = CCFormat.string

  module TokenMap = CCMap.Make(String)

  (** A prefix tree of uniform length *)
  type t =
    | Empty
    | Leaf of int TokenMap.t * int (* token -> weight *)
    | Node of t TokenMap.t * int (* total weight *)

  let empty = Empty

  let singleton v = Leaf (TokenMap.singleton v 1, 1)

  (* add [key_l -> v] *)
  let rec add key_l v t = match key_l with
    | [] ->
      begin match t with
        | Empty -> singleton v
        | Leaf (m,w) ->
          let m = TokenMap.add v (TokenMap.get_or v m ~default:0 + 1) m in
          Leaf (m,w+1)
        | Node _ -> invalid_arg "trie is not of uniform depth"
      end
    | k :: key_tail ->
      begin match t with
        | Empty -> Node (TokenMap.singleton k (add key_tail v Empty), 1)
        | Leaf _ -> invalid_arg "trie is not of uniform depth"
        | Node (m,w) ->
          let sub = TokenMap.get_or k m ~default:Empty in
          let sub = add key_tail v sub in
          Node (TokenMap.add k sub m, w+1)
      end

  let rec merge a b = match a, b with
    | Empty, b -> b
    | a, Empty -> a
    | Leaf (m1,w1), Leaf (m2,w2) ->
      let m =
        TokenMap.merge_safe m1 m2
          ~f:(fun _ v -> match v with
            | `Left x | `Right x -> Some x
            | `Both (x,y) -> Some (x+y))
      in
      Leaf (m,w1+w2)
    | Node (m1,w1), Node (m2,w2) ->
      let m =
        TokenMap.merge_safe m1 m2
          ~f:(fun _ v -> match v with
            | `Left x | `Right x -> Some x
            | `Both (x,y) -> Some (merge x y))
      in
      Node (m, w1+w2)
    | Leaf _, Node _
    | Node _, Leaf _ -> invalid_arg "tries are not of same depth"

  let merge_list = function
    | [] -> empty
    | a :: l -> List.fold_left merge a l

  let get_weight = function
    | Empty -> 0
    | Leaf (_, w)
    | Node (_, w) -> w

  (* skip [i] weighted elements from [m].
      @param get_w how to compute the weight of individual elements *)
  let map_weight_get_ ~get_w i m =
    let i = ref i in
    TokenMap.to_seq m
    |> Iter.find
      (fun sub ->
         let w_sub = get_w sub in
         if w_sub <= !i then (
           i := !i - w_sub;
           None
         ) else Some sub)
    |> (function
      | None -> assert false
      | Some res -> res)

  let mem k t : bool = match t with
    | Leaf _
    | Empty -> false
    | Node (m,_) -> TokenMap.mem k m

  let pick_key rand t = match t with
    | Empty -> raise Not_found
    | Leaf _ -> invalid_arg "pick_key: at leaf"
    | Node (m,w) ->
      assert (w>0);
      let i = Random.State.int rand w in
      let t, _ = map_weight_get_ ~get_w:(fun (_,m) -> get_weight m) i m in
      t

  (* sub-tree has weight [w], we might instead do a random jump with
     a probability that increases when [w] becomes small.
  
     [p_jump = exp   *)
  let jump_proba w =
    let w = float w in
    exp ((-. w) /. 0.5)

  let max_jumps = 2

  let pick rand toks0 t_root : token =
    let jumps = ref 0 in
    let rec aux toks t = match toks with
      | [] ->
        begin match t with
          | Empty -> raise Not_found
          | Leaf (m,w) ->
            assert (w>0);
            let i = Random.State.int rand w in
            let t, _ = map_weight_get_ ~get_w:snd i m in
            t
          | Node (m,w) ->
            assert (w>0);
            let i = Random.State.int rand w in
            let _, sub = map_weight_get_ ~get_w:(fun (_,m) -> get_weight m) i m in
            aux [] sub
        end
      | tok :: toks_tail ->
        begin match t with
          | Empty -> raise Not_found
          | Node (m,w) ->
            if !jumps < max_jumps
            && Random.State.float rand 1. < jump_proba w
            then (
              (* random jump *)
              incr jumps;
              let new_tok = pick_key rand t in
              aux (new_tok::toks_tail) t
            ) else begin match TokenMap.get tok m with
              | Some sub -> aux toks_tail sub
              | None ->
                (* forced jump *)
                let new_tok = pick_key rand t in
                aux (new_tok::toks_tail) t
            end
          | Leaf _ -> invalid_arg "pick: wrong depth"
        end
    in
    aux toks0 t_root

  let rec print out t = match t with
    | Empty -> ()
    | Leaf (m,_) ->
      let pp_pair out (tok,i) =
        Format.fprintf out "@[<h>%a (weight %d)@]" print_token tok i
      in
      Format.fprintf out "@[<v>%a@]"
        (CCFormat.seq pp_pair)
        (TokenMap.to_seq m)
    | Node (m,w) ->
      let pp_pair out (tok,m) =
        Format.fprintf out "@[<v1>%a (weight %d): %a@]" print_token tok w print m
      in
      Format.fprintf out "@[<v>%a@]"
        (CCFormat.seq pp_pair)
        (TokenMap.to_seq m)

  let write_to out t =
    Marshal.to_channel out t [];
    flush out

  let read_from ic =
    Marshal.from_channel ic
end

(** {2 Parse IRC logs into a table} *)
module Parse_logs = struct
  module I = Irclog
  module T = Table

  let norm_token =
    let b = Buffer.create 256 in
    fun s ->
      String.iter
        (fun c -> match c with
           | 'a'..'z' -> Buffer.add_char b c
           | 'A'..'Z' -> Buffer.add_char b (CCChar.lowercase_ascii c)
           | '\t' | ' ' | '.' | ',' -> assert false
           | c -> Buffer.add_char b c)
        s;
      let res = Buffer.contents b in
      Buffer.clear b;
      res

  let re_split = Re.Posix.compile_pat "[ \t,.?!]"

  (* tokenize the string *)
  let tokenize s =
    let l =
      Re.split re_split s
      |> List.filter (fun s -> s<>"")
      |> List.rev_map (fun s -> norm_token s)
    in
    "" :: "" :: List.rev l

  (* parse record [r] into [tbl] *)
  let parse_record r tbl =
    let author = I.norm_author r.I.author in
    let tokens = tokenize r.I.msg in
    let rec aux toks t = match toks with
      | [] | [_] | [_;_] -> t
      | t1 :: ((t2 :: next :: _) as toks') ->
        let t = T.add [author; t1; t2] next t in
        aux toks' t
    in
    aux tokens tbl

  (* parse the file and add it to the table *)
  let parse_file fmt file tbl =
    I.iter_file fmt file
    |> Iter.fold (fun tbl r -> parse_record r tbl) tbl

  let parse_dir fmt dir tbl =
    I.iter_dir fmt dir
    |> Iter.fold (fun tbl (_file,r) -> parse_record r tbl) tbl

  let parse_file_or_dir fmt name tbl =
    I.iter_file_or_dir fmt name
    |> Iter.fold (fun tbl r -> parse_record r tbl) tbl
end

(** {2 Generate} *)
module Gen = struct
  module T = Table

  let gen_rec rand min_len prefix tbl =
    let rec gen acc p1 p2 =
      if List.length acc >= min_len && Random.State.int rand 10 < 1
      then String.concat " " (List.rev acc) (* stop *)
      else match T.pick rand [prefix; p1; p2] tbl with
        | "" -> assert false
        | w ->
          (* shift: p1, p2 = p2, w *)
          gen (w :: acc) p2 w
    in
    gen [] "" ""

  (* pick an author from [tbl] *)
  let pick_author rand tbl = T.pick_key rand tbl

  let default_rand_ = Random.State.make_self_init()

  (* generate a sentence from the given author *)
  let generate ?author ?(rand=default_rand_) ?(min_len=10) tbl =
    let prefix = match author with
      | Some a when T.mem a tbl -> Irclog.norm_author a
      | _ -> pick_author rand tbl
    in
    gen_rec rand min_len prefix tbl
end

(** {2 Plugin} *)

type state = {
  tbl: Table.t;
  rand: Random.State.t;
}

let cmd_markov (state:state): Command.t =
  Command.make_simple
    ~descr:"generate random chains" ~prio:15 ~cmd:"markcough"
    (fun _ msg ->
       let msg = String.trim msg in
       let author = if msg="" then None else Some msg in
       Lwt.return_some (Gen.generate ?author ~rand:state.rand state.tbl)
    )

(* initialization *)
let of_json _ _ : state Lwt_err.t =
  (* TODO: use non blocking IO *)
  let tbl =
    try CCIO.with_in "markcough.state" Table.read_from
    with _ -> Table.empty
  in
  let state = {
    tbl;
    rand=Random.State.make_self_init();
  } in
  Logs.debug (fun k->k  "markcough: parsed state, weight %d" (Table.get_weight tbl));
  Lwt_err.return state

let plugin =
  Plugin.stateful
    ~name:"markcough"
    ~to_json:(fun _ -> None)
    ~of_json
    ~commands:(fun state -> [cmd_markov state])
    ()
