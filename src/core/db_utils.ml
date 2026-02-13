module DB = Sqlite3

(* check DB errors *)
let[@inline] check_db_ db rc =
  if DB.Rc.is_success rc || rc = DB.Rc.ROW then
    ()
  else
    failwith
      (Printf.sprintf "DB error: %s %s" (DB.Rc.to_string rc) (DB.errmsg db))

let with_stmt db q f =
  let stmt = DB.prepare db q in
  let close () = DB.finalize stmt |> check_db_ db in
  try
    let x = f stmt in
    close ();
    x
  with e ->
    close ();
    raise e
