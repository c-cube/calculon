
module DB = Sqlite3

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
       );
    |} |> check_db_;
  Printf.printf "creating index\n";
  DB.exec db {|
    CREATE INDEX IF NOT EXISTS plugins_idx on plugins(name);
    |} |> check_db_;
  let j = Yojson.Safe.from_file !file in
  begin match j with
    | `Assoc l ->
      DB.exec db "BEGIN;" |> check_db_;

      let stmt = DB.prepare db "INSERT INTO plugins(name,value) VALUES (?,?);" in
      List.iter (fun (n,sub) ->
          Printf.printf "inserting %s\n%!" n;
          let sub = Yojson.Safe.to_string sub in
          DB.reset stmt |> check_db_;
          DB.bind_text stmt 1 n |> check_db_;
          DB.bind_text stmt 2 sub |> check_db_;
          DB.step stmt |> check_db_;
        ) l;

      Printf.printf "commit\n%!";
      DB.exec db "COMMIT;" |> check_db_;

    | _ -> failwith "expected json to be an object"
  end;

  ()
