
(* This file is free software. See file "license" for more details. *)

module CE = Calculon_extras
module M = CE.Plugin_markcough

(** {1 Learn Tool} *)

let parse_file fmt name =
  Format.printf "parse file `%s`@." name;
  let res = M.Parse_logs.parse_file_or_dir fmt name M.Table.empty in
  Format.printf "parsing of `%s` done@." name;
  res

let build_table fmt l =
  let tbls = List.map (parse_file fmt) l in
  M.Table.merge_list tbls

let main ~res fmt files =
  let table = build_table fmt files in
  CCIO.with_out res
    (fun oc -> M.Table.write_to oc table)

let () =
  let res = ref "markcough.state" in
  let files = ref [] in
  let fmt = ref "weechat" in
  let options =
    Arg.align
      [ "-o", Arg.Set_string res, " set output file, to write the table into";
        "--fmt", Arg.Symbol (CE.Irclog.fmt_l, fun s->fmt := s), " input format";
      ]
  in
  Arg.parse options (fun f->files:= f:: !files) "learn -o <res> <files>";
  main ~res:!res (CE.Irclog.fmt_of_string !fmt) !files
