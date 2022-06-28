open Lwt_infix

let main ?cmd_help conf all : unit Lwt.t =
  let init_or_err (core : Core.t) : _ result Lwt.t =
    let (module C) = core in
    (* setup plugins *)
    let plugins =
      Plugin.Set.create ?cmd_help conf all |> unwrap_result_failwith
    in
    (* connect to chan *)
    let* () =
      Lwt_list.iter_p (fun c -> C.send_join ~channel:c) conf.Config.channels
    in
    Logs.info ~src:Core.logs_src (fun k -> k "run %d plugins" (List.length all));
    (* log incoming messages, apply commands to them *)
    let prefix = conf.Config.prefix in
    Signal.on' C.messages (fun msg ->
        let cmds = Plugin.Set.commands plugins in
        let on_msg_l = Plugin.Set.on_msg_l plugins in
        let* () = Lwt_list.iter_s (fun f -> f core msg) on_msg_l in
        match Core.privmsg_of_msg msg with
        | None -> Lwt.return ()
        | Some msg -> Command.run ~prefix core cmds msg);
    Lwt.return @@ Ok ()
  in
  (* error-logging wraper *)
  let init core : unit Lwt.t =
    let+ x = init_or_err core in
    match x with
    | Ok () -> ()
    | Error (Failure msg) ->
      Logs.err ~src:Core.logs_src (fun k -> k "error in main loop: %s" msg)
    | Error e ->
      let msg = Printexc.to_string e in
      Logs.err ~src:Core.logs_src (fun k -> k "error in main loop: %s" msg)
  in
  Core.run conf ~init ()
