
module DB = Sqlite3
module J = Yojson.Safe

let help=
  {|migrate_state state.json [opt]*

This tool migrates a state.json file into a database (sqlite) file
that Calculon now uses.|}

let () =
  let file = ref "" in
  let db = ref Calculon.Config.default.db_file in
  let opts = [
    "-db", Arg.Set_string db, " DB file";
  ] |> Arg.align in
  Arg.parse opts (fun f -> file := f) help;
  if !file="" then failwith "please provide a file";
  Printf.printf "migrating %S to %S\n" !file !db;

  let db = DB.db_open !db in

  let[@inline] check_db_ rc =
    if DB.Rc.is_success rc then ()
    else failwith (Printf.sprintf "DB error: %s %s" (DB.Rc.to_string rc) (DB.errmsg db))
  in

  Printf.printf "creating table\n";
  DB.exec db {|
    CREATE TABLE IF NOT EXISTS plugins
      (name TEXT NOT NULL,
       value TEXT NOT NULL,
       UNIQUE (name) ON CONFLICT FAIL
       ) STRICT;
    |} |> check_db_;

  Printf.printf "creating index\n";
  DB.exec db {|
    CREATE INDEX IF NOT EXISTS plugins_idx on plugins(name);
    |} |> check_db_;

  let tr_plugin (n,sub) =
    Printf.printf "inserting %s\n%!" n;
    if n="factoids" then (

      DB.exec db {|
        CREATE TABLE IF NOT EXISTS
          factoids (
            key TEXT NOT NULL,
            value TEXT NOT NULL,
            UNIQUE (key) on CONFLICT FAIL
          ) STRICT;
        |} |> check_db_;

      DB.exec db {|
        CREATE INDEX IF NOT EXISTS factoids_idx on factoids(key);
      |} |> check_db_;

      let l = J.Util.to_assoc sub in
      let stmt = DB.prepare db {| INSERT INTO factoids(key,value) VALUES (?,?); |} in
      List.iter
        (fun (key, value) ->
           let str_val = J.to_string value in

           DB.reset stmt |> check_db_;
           DB.bind_text stmt 1 key |> check_db_;
           DB.bind_text stmt 2 str_val |> check_db_;
           DB.step stmt |> check_db_;
        ) l;

      DB.finalize  stmt |> check_db_;


    ) else if n="social" then (
      DB.exec db
        {| CREATE TABLE IF NOT EXISTS
        social(name TEXT NOT NULL,
               value TEXT NOT NULL,
               UNIQUE (name) ON CONFLICT FAIL
               ) STRICT;
      |} |> check_db_;
      DB.exec db
        {| CREATE INDEX IF NOT EXISTS idx_social on social(name); |}
      |> check_db_;

      let l = J.Util.to_assoc sub in
      (* add value, but be ready to merge if a nick was present several
         times (possibly because of casing inconsistencies) *)
      let stmt = DB.prepare db
          {| INSERT INTO social(name,value) VALUES (?1,?2)
             ON CONFLICT(name) DO
             UPDATE SET value =
              json_object(
                'ignore_user',
                ( CASE json_extract(?2, '$.ignore_user')
                       OR json_extract(value, '$.ignore_user')
                  WHEN 1 THEN json('true')
                  WHEN 0 THEN json('false') END),
                'lastSeen',
                max(json_extract(?2, '$.lastSeen'), json_extract(value, '$.lastSeen')),
                'to_tell',
                (SELECT json_group_array(x.value) FROM
                  ( SELECT value FROM json_each(value, '$.tell') UNION
                    SELECT value FROM json_each(?2, '$.tell' ))
                  as x)
              );
             |} in
      List.iter
        (fun (name, value) ->
           let str_val = J.to_string value in
           let name = String.lowercase_ascii name in

           DB.reset stmt |> check_db_;
           DB.bind_text stmt 1 name |> check_db_;
           DB.bind_text stmt 2 str_val |> check_db_;
           DB.step stmt |> check_db_;
        ) l;

      DB.finalize  stmt |> check_db_;


    ) else (
      let sub = J.to_string sub in
      let stmt = DB.prepare db "INSERT INTO plugins(name,value) VALUES (?,?);" in
      DB.bind_text stmt 1 n |> check_db_;
      DB.bind_text stmt 2 sub |> check_db_;
      DB.step stmt |> check_db_;
      DB.finalize stmt |> check_db_;
    )
  in

  let j = Yojson.Safe.from_file !file in
  begin match j with
    | `Assoc l ->
      DB.exec db "BEGIN;" |> check_db_;

      List.iter tr_plugin l;

      Printf.printf "commit\n%!";
      DB.exec db "COMMIT;" |> check_db_;

    | _ -> failwith "expected json to be an object"
  end;

  ()
