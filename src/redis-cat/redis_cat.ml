
open Lwt.Infix
module CR = Calculon_redis_lib
module R = CR.Client

let cat_in (c:R.t) ic ~targets =
  Lwt_io.read_lines ic
  |> Lwt_stream.iter_s
    (fun s ->
       let s = String.trim s in
       Lwt_list.iter_p
         (fun dest ->
            let open CR.Message_t in
            let msg = {message=`Privmsg {dest; source="redis-cat"; message=s}} in
            R.send c msg)
         targets)

let main ~stdin ~files ~targets ~debug () : unit Lwt.t =
  R.make ~debug () >>= function
  | Error msg -> Lwt.fail_with ("error: " ^ msg)
  | Ok c ->
    if targets=[] || (not stdin && files=[]) then (
      (* read *)
      R.on_received c
        (fun msg ->
          Lwt_io.printlf "received %s" (CR.Message_j.string_of_message msg));
      R.join c
    ) else (
      (* write *)
      let stdin = if stdin then cat_in c ~targets Lwt_io.stdin else Lwt.return () in
      let files =
        Lwt_list.map_s
          (fun file ->
            Lwt_io.with_file ~mode:Lwt_io.input file
               (fun ic -> cat_in c ~targets ic))
          files
        >|= fun _ -> ()
      in
      Lwt.join [stdin; files] >>= fun () ->
      R.close c
    )

let () =
  let targets = ref [] in
  let debug_ = ref false in
  let files = ref [] in
  let stdin = ref false in
  let opts = [
    "--debug", Arg.Set debug_, " enable debug";
    "--to", Arg.String (fun c -> targets := c :: !targets), " pipe into given target";
    "--stdin", Arg.Set stdin, " pipe stdin into IRC (see: --to)";
    "-f", Arg.String (fun f -> files := f :: !files), " files to pipe into IRC";
  ] |> Arg.align in
  Arg.parse opts (fun _ -> ())
    "usage: redis-cat [option*]\n\nlistens to incoming redis messages, pipe stdin to redis";
  Lwt_main.run (main ~stdin:!stdin ~files:!files ~targets:!targets ~debug:!debug_ ())

