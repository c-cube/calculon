open Prelude
open DB_utils
module Log = Core.Log

type key = string
type value = StrList of string list | Int of int
type factoid = { key: key; value: value }
type json = Yojson.Safe.t
type t = DB.db

type op =
  | Get of key
  | Set of factoid
  | Set_force of factoid
  | Append of factoid
  | Remove of factoid
  | Incr of key
  | Decr of key

let key_of_string s : key option =
  let s = String.trim s in
  if String.contains s ' ' then
    None
  else
    Some s

let string_of_value = function
  | Int i -> string_of_int i
  | StrList l -> Prelude.string_list_to_string l

let string_of_op = function
  | Get k -> "get " ^ k
  | Set { key; value } -> "set " ^ key ^ " = " ^ string_of_value value
  | Set_force { key; value } ->
    "set_force " ^ key ^ " := " ^ string_of_value value
  | Append { key; value } -> "append " ^ key ^ " += " ^ string_of_value value
  | Remove { key; value } -> "remove " ^ key ^ " -= " ^ string_of_value value
  | Incr k -> "incr " ^ k
  | Decr k -> "decr " ^ k

let mk_key key =
  match key_of_string key with
  | None -> invalid_arg ("mk_key : `" ^ key ^ "`")
  | Some key -> key

let mk_factoid key value =
  let key = mk_key key in
  let value = String.trim value in
  try { key; value = Int (int_of_string value) }
  with Failure _ -> { key; value = StrList [ value ] }

(* joins the result of Re_perl.split_full back together*)
let group_join list =
  let buf = Buffer.create 80 in
  let rec aux = function
    | `Text t :: r ->
      Buffer.add_string buf t;
      aux r
    | `Delim d :: r ->
      Buffer.add_string buf (Re.Group.get d 0);
      aux r
    | [] -> Buffer.contents buf
  in
  aux list

let re_split_pat = Re.Perl.compile_pat "(^!)|([+-:]?=)|(\\+\\+)|(--)"
let re_factoid = Re.Perl.compile_pat "^[ ]*[^ \n\t]+[ ]*$"

let parse_op ~prefix msg : (op * string option) option =
  let open Option.Infix in
  let msg, hl =
    match Command.extract_hl msg with
    | None -> msg, None
    | Some (a, b) -> a, Some b
  in
  let open Option in
  let mk_get k = Get (mk_key k) in
  let mk_set k v = Set (mk_factoid k v) in
  let mk_set_force k v = Set_force (mk_factoid k v) in
  let mk_append k v = Append (mk_factoid k v) in
  let mk_remove k v = Remove (mk_factoid k v) in
  let mk_incr k = Incr (mk_key k) in
  let mk_decr k = Decr (mk_key k) in
  let is_command l = String.equal prefix (Re.Group.get l 0) in
  let is_factoid f =
    match Re.exec_opt re_factoid f with
    | None -> false
    | Some _ -> true
  in
  if String.contains msg '\x01' then
    None
  else
    Re.split_full re_split_pat msg
    |> ( function
    | `Delim prefix :: `Text factoid :: `Delim op :: rest
      when is_command prefix && is_factoid factoid ->
      let op = Re.Group.get op 0 in
      let fact = group_join rest |> String.trim in
      let tfactoid = String.trim factoid in
      some (op, tfactoid, fact)
    | [ `Delim prefix; `Text factoid ]
      when is_command prefix && is_factoid factoid ->
      some ("", String.trim factoid, "")
    | _ -> None )
    >>= ( function
    | "=", factoid, fact -> mk_set factoid fact |> some
    | "+=", factoid, fact -> mk_append factoid fact |> some
    | "-=", factoid, fact -> mk_remove factoid fact |> some
    | ":=", factoid, fact -> mk_set_force factoid fact |> some
    | "++", factoid, "" -> mk_incr factoid |> some
    | "--", factoid, "" -> mk_decr factoid |> some
    | "", factoid, _ -> mk_get factoid |> some
    | _ -> None )
    |> Option.map (fun x -> x, hl)

let () =
  let test_ok s = Option.is_some (parse_op ~prefix:"!" s) in
  assert (test_ok "!foo2 = bar");
  assert (test_ok "!foo2 = bar ");
  assert (test_ok "!foo2 += bar");
  assert (test_ok "!foo2 += bar hello world");
  assert (test_ok "!foo2 -= bar");
  assert (test_ok "!foo2 -= bar hello world");
  assert (test_ok "!foo ++");
  assert (test_ok "!foo ++ ");
  assert (test_ok "!foo --");
  assert (test_ok "!foo -- ");
  assert (test_ok "!foo");
  assert (test_ok "!foo ");
  ()

exception Could_not_parse

let as_str (j : json) : string =
  match j with
  | `String s -> s
  | _ -> raise Could_not_parse

let as_value (j : json) : value =
  match j with
  | `List l -> StrList (List.map as_str l)
  | `Int i -> Int i
  | _ -> raise Could_not_parse

let json_of_value = function
  | StrList l -> `List (List.map (fun s -> `String s) l)
  | Int i -> `Int i

let get ?(default = StrList []) key (self : t) : value =
  let@ () = wrap_failwith "factoids.get" in
  let@ stmt =
    with_stmt self {| SELECT json(value) FROM factoids WHERE key=? |}
  in
  DB.bind_text stmt 1 key |> check_db_ self;
  DB.step stmt |> check_db_ self;
  try
    let j = DB.column_text stmt 0 in
    as_value @@ Yojson.Safe.from_string j
  with _ -> default

let mem key (self : t) : bool =
  let@ () = wrap_failwith "factoids.mem" in
  let@ stmt =
    with_stmt self {| SELECT EXISTS (SELECT(value) FROM factoids WHERE key=?) |}
  in
  DB.bind_text stmt 1 key |> check_db_ self;
  DB.step stmt |> check_db_ self;
  DB.column_bool stmt 0

let set { key; value } (self : t) : unit =
  let@ () = wrap_failwith "factoids.set" in
  let v = json_of_value value |> Yojson.Safe.to_string in
  let@ stmt =
    with_stmt self
      {| INSERT INTO factoids(key,value) VALUES(?1,?2)
         ON CONFLICT DO UPDATE SET value=?2 |}
  in
  DB.bind_text stmt 1 key |> check_db_ self;
  DB.bind_text stmt 2 v |> check_db_ self;
  DB.step stmt |> check_db_ self;
  ()

let append { key; value } (self : t) : unit =
  let@ () = wrap_failwith "factoids.append" in
  DB.exec self "BEGIN;" |> check_db_ self;
  let value' =
    match get key self, value with
    | Int i, Int j -> Int (i + j)
    | StrList [], _ -> value
    | StrList l, StrList l' -> StrList (l @ l')
    | StrList l, Int j -> StrList (string_of_int j :: l)
    | Int i, StrList l -> StrList (string_of_int i :: l)
  in
  set { key; value = value' } self;
  DB.exec self "COMMIT" |> check_db_ self;
  ()

let remove { key; value } (self : t) : unit =
  let@ () = wrap_failwith "factoids.remove" in
  let value' =
    match get key self, value with
    | Int i, Int j -> Int (i - j)
    | StrList [], Int j -> Int (-j)
    | StrList [], _ -> value
    | StrList l, StrList l' ->
      StrList (List.filter (fun s -> not (List.exists (String.equal s) l')) l)
    | StrList l, Int j ->
      StrList (List.filter (fun s -> not (String.equal (string_of_int j) s)) l)
    | Int _, StrList _ ->
      Printf.printf "Hé non, on enlève pas des strings à une valeur entière !";
      value
  in
  match value' with
  | StrList [] | Int 0 ->
    let@ stmt = with_stmt self {| DELETE FROM factoids WHERE key=? |} in
    DB.bind_text stmt 1 key |> check_db_ self;
    DB.step stmt |> check_db_ self
  | _ -> set { key; value = value' } self

let as_int v =
  match v with
  | Int i -> Some i
  | StrList [ s ] -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let incr key (self : t) : int option =
  let value = get key ~default:(Int 0) self in
  match as_int value with
  | Some i ->
    let count = i + 1 in
    set { key; value = Int count } self;
    Some count
  | None -> None

let decr key (self : t) : int option =
  let value = get key ~default:(Int 0) self in
  match as_int value with
  | Some i ->
    let count = i - 1 in
    set { key; value = Int count } self;
    Some count
  | None -> None

let search tokens (self : t) : string list =
  let tokens = List.map CCString.lowercase_ascii tokens in
  (* list pairs [key, value] that match all the given tokens? *)
  let check_str s tok = CCString.mem ~sub:tok (CCString.lowercase_ascii s) in
  let check_int i tok = String.equal tok (string_of_int i) in
  let mk_pair k v = Printf.sprintf "%s -> %s" k v in
  let tok_matches key value : string list =
    match value with
    | Int i ->
      if List.for_all (fun tok -> check_str key tok || check_int i tok) tokens
      then
        [ mk_pair key (string_of_int i) ]
      else
        []
    | StrList l ->
      CCList.filter_map
        (fun sub ->
          if
            List.for_all
              (fun tok -> check_str key tok || check_str sub tok)
              tokens
          then
            Some (mk_pair key sub)
          else
            None)
        l
  in

  let@ stmt = with_stmt self {| SELECT key, value FROM factoids; |} in
  let rc, l =
    DB.fold stmt ~init:[] ~f:(fun choices row ->
        match row with
        | [| DB.Data.TEXT key; DB.Data.TEXT value |] ->
          let value = Yojson.Safe.from_string value |> as_value in
          List.rev_append (tok_matches key value) choices
        | _ -> choices)
  in
  check_db_ self rc;
  l

let random (self : t) : string =
  let@ () = wrap_failwith "factoids.random" in
  match
    let@ stmt =
      with_stmt self {| SELECT key FROM factoids ORDER BY random() LIMIT 1 |}
    in
    DB.step stmt |> check_db_ self;
    try Some (DB.column_text stmt 0) with _ -> None
  with
  | None -> ""
  | Some key ->
    Log.debug (fun k -> k "random: key is %S" key);
    let value = get key self in
    let msg_val =
      match value with
      | StrList [] -> assert false
      | StrList l -> Rand_distrib.uniform l |> Rand_distrib.run
      | Int i -> string_of_int i
    in
    spf "!%s: %s" key msg_val

(* returns a help message that suggest keys that are close to [k]
   by edit distance, and the number of such keys *)
let find_close_keys (k : key) (self : t) : string * int =
  let l =
    let@ stmt = with_stmt self {| SELECT key FROM factoids |} in
    let rc, l =
      DB.fold stmt ~init:[] ~f:(fun keys row ->
          match row with
          | [| DB.Data.TEXT key |] ->
            let d = Prelude.edit_distance key k in
            if d <= 2 then
              (d, key) :: keys
            else
              keys
          | _ -> keys)
    in
    check_db_ self rc;
    l |> List.sort CCOrd.poly |> List.map snd
  in
  let l =
    if List.length l > 5 then
      CCList.take 5 l @ [ "…" ]
    else
      l
  in
  let res =
    match l with
    | [] -> ""
    | [ x ] -> Printf.sprintf "did you mean %s?" x
    | _ ->
      CCFormat.sprintf "did you mean one of %a@]?" CCFormat.(Dump.list string) l
  in
  res, List.length l

(* operations *)

let max_card_for_force = ref 5

let set_max_cardinal_for_force x =
  assert (x >= 2);
  max_card_for_force := x

(* maximum size of returned lists *)
let list_size_limit = 4

let limit_list l =
  let n = List.length l in
  if n > list_size_limit then
    CCList.take list_size_limit l @ [ "…" ]
  else
    l

let insert_noresult = function
  | [] -> [ "nothing found!" ]
  | l -> l

(* tokenize message into search tokens *)
let search_tokenize s = String.trim s |> Re.split (Re.Perl.compile_pat "[ \t]+")

let cmd_search (self : t) =
  Command.make_simple_l ~descr:"search in factoids" ~cmd:"search" ~prio:10
    (fun _ s ->
      let tokens = search_tokenize s in
      search tokens self |> limit_list |> insert_noresult |> Lwt.return)

let cmd_search_all (self : t) =
  Command.make_simple_query_l
    ~descr:"search all matches in factoids (reply in pv)" ~cmd:"search_all"
    ~prio:10 (fun _ s ->
      Lwt.return
      @@
      let tokens = search_tokenize s in
      search tokens self |> insert_noresult |> fun l ->
      if List.length l > 5 then
        [ String.concat " | " l ]
      else
        l)

let cmd_see (self : t) =
  Command.make_simple_l ~descr:"see a factoid's content" ~cmd:"see" ~prio:10
    (fun _ s ->
      let v = get (mk_key s) self in
      let msg =
        match v with
        | Int i -> [ string_of_int i ]
        | StrList [] -> [ "not found." ]
        | StrList l -> limit_list l
      in
      Lwt.return msg)

let cmd_see_all (self : t) =
  Command.make_simple_query_l ~descr:"see all of a factoid's content (in pv)"
    ~cmd:"see_all" ~prio:10 (fun _ s ->
      let v = get (mk_key s) self in
      let msg =
        match v with
        | Int i -> [ string_of_int i ]
        | StrList [] -> [ "not found." ]
        | StrList l ->
          if List.length l > 5 then
            [ String.concat " | " l ]
          else
            l
      in
      Lwt.return msg)

let cmd_random (self : t) =
  Command.make_simple ~descr:"random factoid" ~cmd:"random" ~prio:10 (fun _ _ ->
      let msg = random self in
      Lwt.return @@ Some msg)

let cmd_factoids (self : t) =
  let reply ~prefix (module C : Core.S) msg =
    let target = Core.reply_to msg in
    let matched x = Command.Cmd_match x in
    let add_hl hl line =
      match hl with
      | None -> line
      | Some x -> x ^ ": " ^ line
    in
    let reply_value ~hl (v : value) =
      match v with
      | Int i ->
        C.send_privmsg ~target ~message:(string_of_int i |> add_hl hl)
        |> matched
      | StrList [] -> matched @@ Lwt.return ()
      | StrList [ message ] ->
        C.send_privmsg ~target ~message:(add_hl hl message) |> matched
      | StrList l ->
        let message = Rand_distrib.uniform l |> Rand_distrib.run |> add_hl hl in
        C.send_privmsg ~target ~message |> matched
    and count_update_message (k : key) = function
      | None -> Lwt.return ()
      | Some count ->
        C.send_privmsg ~target
          ~message:(Printf.sprintf "%s : %d" (k :> string) count)
    in
    let op = parse_op ~prefix msg.Core.message in
    Option.iter
      (fun (c, _) ->
        Log.debug (fun k -> k "factoids: parsed command `%s`" (string_of_op c)))
      op;
    match op with
    | Some (Get k, hl) ->
      (match get k self with
      | StrList [] ->
        let help_msg, n = find_close_keys k self in
        (* probably a typo for this key *)
        if n > 0 then
          C.send_privmsg ~target ~message:help_msg |> matched
        else
          Command.Cmd_skip
      | v ->
        Log.debug (fun k -> k "factoids: get returned %s" (string_of_value v));
        reply_value ~hl v)
    | Some (Set f, _) ->
      if mem f.key self then
        C.talk ~target Talk.Err |> matched
      else (
        set f self;
        C.talk ~target Talk.Ack |> matched
      )
    | Some (Set_force f, _) ->
      let l = get f.key self in
      (match l with
      | StrList l when List.length l >= !max_card_for_force ->
        C.talk ~target Talk.Err |> matched
      | _ ->
        set f self;
        C.talk ~target Talk.Ack |> matched)
    | Some (Append f, _) ->
      append f self;
      C.talk ~target Talk.Ack |> matched
    | Some (Remove f, _) ->
      remove f self;
      C.talk ~target Talk.Ack |> matched
    | Some (Incr k, _) ->
      let count = incr k self in
      count_update_message k count |> matched
    | Some (Decr k, _) ->
      let count = decr k self in
      count_update_message k count |> matched
    | None -> Command.Cmd_skip
  in
  Command.make ~name:"factoids" ~prio:80 reply
    ~descr:
      "factoids, triggered by the following commands:\n\n\
      \    - `!foo` will retrieve one of the factoids associated with `foo`, \
       if any\n\
      \    - `!foo = bar` maps `foo` to `bar`, unless `foo` is mapped yet\n\
      \      (in which case it fails)\n\
      \    - `!foo += bar` adds `bar` to the mappings of `foo`, or adds \
       integer value bar to the integer value foo\n\
      \    - `!foo -= bar` removes `bar` from the mappings of `foo`, or \
       subtracts bar to the integer value foo\n\
      \    - `!foo++` adds 1 to the integer value foo\n\
      \    - `!foo--` subtracts 1 to the integer value foo\n\
      \    - `!foo := bar` maps `foo` to `bar` even if `foo` is already mapped\n\
      \    - `!search term` looks up `term` in the database\n\
      \    - `!search_all` looks up all terms in the database\n\
      \    "

let commands (state : t) : Command.t list =
  [
    cmd_factoids state;
    cmd_search state;
    cmd_search_all state;
    cmd_see state;
    cmd_see_all state;
    cmd_random state;
  ]

let prepare_db db =
  DB.exec db
    {|
    CREATE TABLE IF NOT EXISTS
      factoids (
        key TEXT NOT NULL,
        value TEXT NOT NULL,
        UNIQUE (key) ON CONFLICT FAIL
      ) STRICT;
    |}
  |> check_db_ db;

  DB.exec db
    {|
    CREATE INDEX IF NOT EXISTS factoids_idx on factoids(key);
  |}
  |> check_db_ db;
  ()

let plugin : Plugin.t = Plugin.db_backed ~prepare_db ~commands ()
