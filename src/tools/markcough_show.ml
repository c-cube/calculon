
(* This file is free software. See file "license" for more details. *)

module CE = Calculon_extras
module M = CE.Plugin_markcough

(** {1 Show table} *)

let () =
  let file = ref "markcough.state" in
  let dump = ref false in
  let author = ref "" in
  let () =
    Arg.parse
      (Arg.align
         [ "--file", Arg.Set_string file, " state file";
           "--dump", Arg.Set dump, " dump the whole table";
           "--author", Arg.Set_string author, " author of generated quote";
         ])
      (fun _ -> ()) "markcough_show [--file file] [--gen]"
  in
  Format.printf "parse file `%s`@." !file;
  CCIO.with_in !file
    (fun ic ->
       let table = M.Table.read_from ic in
       if !dump
       then Format.printf "@[<v2>transition table:@ %a@]@." M.Table.print table
       else
         let author = if !author="" then None else Some !author in
         Format.printf "%s@." (M.Gen.generate ?author table)
    )
