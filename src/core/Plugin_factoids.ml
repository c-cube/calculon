open Prelude
open Containers
open Lwt.Infix

type key = string
type value =
  | StrList of string list
  | Int of int
type factoid = {key: key; value: value}
type t = factoid StrMap.t
type json = Yojson.Safe.json

type op =
  | Get of key
  | Set of factoid
  | Set_force of factoid
  | Append of factoid
  | Incr of key
  | Decr of key

let key_of_string s : key option =
  let s = String.trim s in
  if String.contains s ' ' then None
  else Some s

let string_of_value = function
  | Int i -> string_of_int i
  | StrList l -> Prelude.string_list_to_string l

let string_of_op = function
  | Get k -> "get " ^ k
  | Set {key;value} -> "set " ^ key ^ " = " ^ string_of_value value
  | Set_force {key;value} -> "set_force " ^ key ^ " := " ^ string_of_value value
  | Append {key;value} -> "append " ^ key ^ " += " ^ string_of_value value
  | Incr k -> "incr " ^ k
  | Decr k -> "decr " ^ k

let mk_key key =
  match key_of_string key with
  | None -> invalid_arg ("mk_key : `" ^ key ^ "`")
  | Some key -> key

let mk_factoid key value =
  let key = mk_key key in
  let value = String.trim value in
  try {key; value = Int (int_of_string value)}
  with Failure _ -> {key; value = StrList [value]}

(* joins the result of Re_perl.split_full back together*)
let group_join list =
  let buf = Buffer.create 80 in
  let rec aux = function
    | (`Text t) :: r ->
      Buffer.add_string buf t;
      aux  r
    | (`Delim d) :: r ->
      Buffer.add_string buf (Re.Group.get d 0);
      aux r
    | [] -> Buffer.contents buf
  in aux list

let re_split_pat = Re_perl.compile_pat "(^!)|([+:]?=)|(\\+\\+)|(--)"
let re_factoid = Re_perl.compile_pat "^[ ]*[a-zA-Z0-9\\-+_]+[ ]*$"

let parse_op msg : (op * string option) option =
  let msg, hl = match Command.extract_hl msg with
    | None -> msg, None
    | Some (a,b) -> a, Some b
  in
  let open Option in
  let mk_get k = Get (mk_key k) in
  let mk_set k v = Set (mk_factoid k v) in
  let mk_set_force k v = Set_force (mk_factoid k v) in
  let mk_append k v = Append (mk_factoid k v) in
  let mk_incr k = Incr (mk_key k) in
  let mk_decr k = Decr (mk_key k) in
  let is_command prefix = Re.Group.get prefix 0 = "!" in (*TODO: generalize to
                                                           any prefix *)
  let is_factoid f = match Re.exec_opt re_factoid f with
    | None -> false
    | Some _ -> true
  in
  if String.contains msg '\x01' then None
  else
    (
      Re.split_full re_split_pat msg |> (function
          | (`Delim prefix) :: (`Text factoid) :: (`Delim op) :: rest
            when (is_command prefix) && (is_factoid factoid)->
            let op = Re.Group.get op 0 in
            let fact = group_join rest |> String.trim in
            let tfactoid = String.trim factoid in
            return (op, tfactoid, fact )
          | [`Delim prefix; `Text factoid; ]
            when (is_command prefix) && (is_factoid factoid)->
            return ("", String.trim factoid, "")
          | _ -> None
        ) >>= (function
          | ("=",  factoid, fact) -> mk_set factoid fact |> return
          | ("+=", factoid, fact) -> mk_append factoid fact |> return
          | (":=", factoid, fact) -> mk_set_force factoid fact |> return
          | ("++", factoid, "" )  -> mk_incr factoid |> return
          | ("--", factoid, "" )  -> mk_decr factoid |> return
          | ("",   factoid, _ )   -> mk_get factoid |> return
          | _ -> None
        )
    ) |> Prelude.map_opt (fun x->x, hl)

let () =
  let test_ok s = CCOpt.is_some (parse_op s) in
  assert (test_ok "!foo2 = bar");
  assert (test_ok "!foo2 = bar ");
  assert (test_ok "!foo2 += bar");
  assert (test_ok "!foo2 += bar hello world");
  assert (test_ok "!foo ++");
  assert (test_ok "!foo ++ ");
  assert (test_ok "!foo --");
  assert (test_ok "!foo -- ");
  assert (test_ok "!foo");
  assert (test_ok "!foo ");
  ()

(* read the json file *)
let read_json (file:string) : json option Lwt.t =
  Lwt.catch
    (fun () ->
       Lwt_io.with_file ~mode:Lwt_io.input file
         (fun ic ->
            Lwt_io.read ic >|= fun s ->
            try Yojson.Safe.from_string s |> some
            with _ -> None))
    (fun _ -> Lwt.return_none)

exception Could_not_parse

let as_str (j:json) : string = match j with
  | `String s -> s
  | _ -> raise Could_not_parse

let as_value (j: json) : value = match j with
  | `List l -> StrList (List.map as_str l)
  | `Int i -> Int i
  | _ -> raise Could_not_parse

let get key (fcs:t) : value =
  try (StrMap.find key fcs).value
  with Not_found -> StrList []

let mem key (fcs:t) : bool = StrMap.mem key fcs

let set ({key;_} as f) (fcs:t): t =
  StrMap.add key f fcs

let append {key;value} (fcs:t): t =
  let value' = match try Some ((StrMap.find key fcs).value, value) with Not_found -> None with
    | Some (`Int i, Int j) -> Int (i+j)
    | Some (`StrList l, StrList l') -> StrList (l @ l')
    | Some (`StrList l, Int j) -> StrList (string_of_int j :: l)
    | Some (`Int i, StrList l) -> StrList (string_of_int i :: l)
    | None -> value
  in
  StrMap.add key {key; value = value'} fcs

let as_int v = match v with
  | Int i -> Some i
  | StrList [s] -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let incr key (fcs:t): int option * t =
  let value = try (StrMap.find key fcs).value with Not_found -> Int 0 in
  match as_int value with
  | Some i ->
    let count = i + 1 in
    (Some count, StrMap.add key {key; value = Int count} fcs)
  | None -> (None, fcs)

let decr key (fcs:t): int option * t =
  let value = try (StrMap.find key fcs).value with Not_found -> Int 0 in
  match as_int value with
  | Some i ->
    let count = i - 1 in
    (Some count, StrMap.add key {key; value = Int count} fcs)
  | None -> (None, fcs)

let search tokens (fcs:t): string list =
  (* list pairs [key, value] that match all the given tokens? *)
  let check_str s tok = CCString.mem ~sub:tok s in
  let check_int i tok = tok = string_of_int i in
  let mk_pair k v = Printf.sprintf "%s -> %s" k v in
  let tok_matches key value : string list = match value with
    | Int i ->
      if List.for_all (fun tok -> check_str key tok || check_int i tok) tokens
      then [mk_pair key (string_of_int i)]
      else []
    | StrList l ->
      CCList.filter_map
        (fun sub ->
           if List.for_all
               (fun tok -> check_str key tok || check_str sub tok)
               tokens
           then Some (mk_pair key sub)
           else None)
        l
  in
  StrMap.fold
    (fun _ {key; value} choices ->
       List.rev_append (tok_matches key value) choices)
    fcs []

let random (fcs:t): string =
  let l = StrMap.to_list fcs in
  match l with
    | [] -> ""
    | _ ->
      let _, fact = Rand_distrib.uniform l |> Rand_distrib.run in
      let msg_val = match fact.value with
        | StrList [] -> assert false
        | StrList l -> Rand_distrib.uniform l |> Rand_distrib.run
        | Int i -> string_of_int i
      in
      Printf.sprintf "!%s: %s" fact.key msg_val

(* returns a help message that suggest keys that are close to [k]
   by edit distance, and the number of such keys *)
let find_close_keys (k:key) (fcs:t) : string * int =
  let l =
    StrMap.fold
      (fun _ {key; _} keys ->
         let d = Prelude.edit_distance key k in
         if d <= 2 then (key, d) :: keys else keys)
      fcs []
    |> List.sort (fun (_, x) (_, y) -> compare x y)
    |> List.map fst
  in
  let l = if List.length l > 5 then CCList.take 5 l @ ["…"] else l in
  let res = match l with
    | [] -> ""
    | [x] -> Printf.sprintf "did you mean %s?" x
    | _ -> CCFormat.sprintf "did you mean one of %a@]?"
             CCFormat.(Dump.list string) l
  in
  res, List.length l

let of_json_exn j =
  begin match j with
    | `Assoc l ->
      List.fold_left
        (fun acc (k, v) ->
           let v = as_value v in
           let key = match key_of_string k with
             | Some k -> k
             | None -> raise Could_not_parse
           in
           append {key;value=v} acc
        )
        StrMap.empty l
    | _ -> raise Could_not_parse
  end

(* parsing/outputting the factoids json *)
let factoids_of_json (j: json): (t,string) CCResult.t =
  try CCResult.return (of_json_exn j)
  with Could_not_parse -> CCResult.fail "could not parse json"

let json_of_factoids (factoids: t): json =
  let l =
    StrMap.fold
      (fun _ {key; value} acc ->
         let jvalue = match value with
           | StrList l -> `List (List.map (fun s -> `String s) l)
           | Int i -> `Int i in
         (key, jvalue) :: acc)
      factoids
      []
  in
  `Assoc l

(* operations *)

let empty = StrMap.empty

type state = {
  mutable st_cur: t;
  actions: Plugin.action Signal.Send_ref.t;
}

let pick_list (l:'a list): 'a option = match l with
  | [] -> None
  | [message] -> Some message
  | l -> Some (Rand_distrib.uniform l |> Rand_distrib.run)

let msg_of_value_pick (v:value): string option = match v with
  | Int i -> Some (string_of_int i)
  | StrList l -> pick_list l

(* maximum size of returned lists *)
let list_size_limit = 4

let limit_list l =
  let n = List.length l in
  if n > list_size_limit
  then CCList.take list_size_limit l @ ["…"]
  else l

let insert_noresult = function
  | [] -> ["nothing found!"]
  | l -> l

(* tokenize message into search tokens *)
let search_tokenize s =
  String.trim s
  |> Re.split (Re_perl.compile_pat "[ \t]+")

let cmd_search state =
  Command.make_simple_l ~descr:"search in factoids" ~prefix:"search" ~prio:10
    (fun _ s ->
       let tokens = search_tokenize s in
       search tokens state.st_cur
       |> limit_list
       |> insert_noresult
       |> Lwt.return
    )

let cmd_search_all state =
  Command.make_simple_query_l
    ~descr:"search all matches in factoids (reply in pv)"
    ~prefix:"search_all" ~prio:10
    (fun _ s ->
       let tokens = search_tokenize s in
       search tokens state.st_cur
       |> insert_noresult
       |> Lwt.return
    )

let cmd_see state =
  Command.make_simple_l ~descr:"see a factoid's content" ~prefix:"see" ~prio:10
    (fun _ s ->
       let v = get (mk_key s) state.st_cur in
       let msg = match v with
         | Int i -> [string_of_int i]
         | StrList [] -> ["not found."]
         | StrList l -> limit_list l
       in
       Lwt.return msg
    )

let cmd_see_all state =
  Command.make_simple_query_l
    ~descr:"see all of a factoid's content (in pv)"
    ~prefix:"see_all"
    ~prio:10
    (fun _ s ->
       let v = get (mk_key s) state.st_cur in
       let msg = match v with
         | Int i -> [string_of_int i]
         | StrList [] -> ["not found."]
         | StrList l -> l
       in
       Lwt.return msg
    )

let cmd_random state =
  Command.make_simple ~descr:"random factoid" ~prefix:"random" ~prio:10
    (fun _ _ ->
       let msg = random state.st_cur in
       Some msg |> Lwt.return
    )

let save state =
  Signal.Send_ref.send state.actions Plugin.Require_save

let cmd_factoids state =
  let reply (module C:Core.S) msg =
    let target = Core.reply_to msg in
    let matched x = Command.Cmd_match x in
    let add_hl hl line = match hl with
      | None -> line
      | Some x -> x ^ ": " ^ line
    in
    let reply_value ~hl (v:value) = match v with
      | Int i ->
        C.send_privmsg ~target ~message:(string_of_int i |> add_hl hl) |> matched
      | StrList [] -> Lwt.return_unit |> matched
      | StrList [message] ->
        C.send_privmsg ~target ~message:(add_hl hl message) |> matched
      | StrList l ->
        let message = Rand_distrib.uniform l |> Rand_distrib.run |> add_hl hl in
        C.send_privmsg ~target ~message |> matched
    and count_update_message (k: key) = function
      | None -> Lwt.return_unit
      | Some count ->
        C.send_privmsg ~target
          ~message:(Printf.sprintf "%s : %d" (k :> string) count)
    in
    let op = parse_op msg.Core.message in
    CCOpt.iter (fun (c,_) -> Log.logf "parsed command `%s`" (string_of_op c)) op;
    begin match op with
      | Some (Get k, hl) ->
        begin match get k state.st_cur with
          | StrList [] ->
            let help_msg, n = find_close_keys k state.st_cur in
            (* probably a typo for this key *)
            if n>0
            then C.send_privmsg ~target ~message:help_msg |> matched
            else Command.Cmd_skip
          | v -> reply_value ~hl v
        end
      | Some (Set f, _) ->
        if mem f.key state.st_cur then (
          C.talk ~target Talk.Err |> matched
        ) else (
          state.st_cur <- set f state.st_cur;
          ( Signal.Send_ref.send state.actions Plugin.Require_save
            >>= fun () ->
            C.talk ~target Talk.Ack) |> matched
        )
      | Some (Set_force f, _) ->
        state.st_cur <- set f state.st_cur;
        (save state >>= fun () -> C.talk ~target Talk.Ack) |> matched
      | Some (Append f, _) ->
        state.st_cur <- append f state.st_cur;
        (save state >>= fun () -> C.talk ~target Talk.Ack) |> matched
      | Some (Incr k, _) ->
        let count, state' = incr k state.st_cur in
        state.st_cur <- state';
        (save state >>= fun () -> count_update_message k count) |> matched
      | Some (Decr k, _) ->
        let count, state' = decr k state.st_cur in
        state.st_cur <- state';
        (save state >>= fun () -> count_update_message k count) |> matched
      | None -> Command.Cmd_skip
    end
  in
  Command.make
    ~name:"factoids" ~prio:80 reply
    ~descr:"factoids, triggered by the following commands:

    - `!foo` will retrieve one of the factoids associated with `foo`, if any
    - `!foo = bar` maps `foo` to `bar`, unless `foo` is mapped yet
      (in which case it fails)
    - `!foo += bar` adds `bar` to the mappings of `foo`
    - `!foo := bar` maps `foo` to `bar` even if `foo` is already mapped
    - `!search term` looks up `term` in the database
    - `!search_all` looks up all terms in the database
    "

let commands state: Command.t list =
  [ cmd_factoids state;
    cmd_search state;
    cmd_search_all state;
    cmd_see state;
    cmd_see_all state;
    cmd_random state;
  ]

let of_json actions j: state Lwt_err.t =
  let open Lwt_err in
  begin match j with
    | None -> Lwt_err.return StrMap.empty
    | Some j -> Lwt.return (factoids_of_json j)
  end
  >|= fun t -> {st_cur=t; actions}

let to_json (st:state): json option =
  Some (json_of_factoids st.st_cur)

let plugin : Plugin.t =
  Plugin.stateful
    ~name:"factoids" ~to_json ~of_json ~commands
    ~stop:(fun _ -> Lwt.return_unit) ()
