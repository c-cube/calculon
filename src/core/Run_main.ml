
let main ?cmd_help conf all : unit =
  let init_or_err (core:Core.t): unit =
    let (module C) = core in
    (* setup plugins *)
    let plugins = Plugin.Set.create ?cmd_help conf all |> unwrap_result_failwith in
    (* connect to chan *)
    C.send_join ~channel:conf.Config.channel;
    Logs.info ~src:Core.logs_src (fun k->k "run %d plugins" (List.length all));
    (* log incoming messages, apply commands to them *)
    let prefix = conf.Config.prefix in
    Signal.on' C.messages
      (fun msg ->
         let cmds = Plugin.Set.commands plugins in
         let on_msg_l = Plugin.Set.on_msg_l plugins in
         List.iter (fun f -> f core msg) on_msg_l;
         match Core.privmsg_of_msg msg with
           | None -> ()
           | Some msg -> Command.run ~prefix core cmds msg);
    ()
  in
  (* error-logging wraper *)
  let init core : unit =
    try
      init_or_err core
    with
    | Failure msg ->
      Logs.err ~src:Core.logs_src (fun k->k "error in main loop: %s" msg);
    | e ->
      let msg = Printexc.to_string e in
      Logs.err ~src:Core.logs_src (fun k->k "error in main loop: %s" msg);
  in
  Core.run conf ~init ()
