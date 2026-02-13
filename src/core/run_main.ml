let main ?cmd_help ?(on_init = ignore) ~sw ~(net : Eio_unix.Net.t)
    ~(clock : _ Eio.Time.clock) conf all : unit =
  let plugins_ref : Plugin.Set.t option ref = ref None in
  let on_connect (core : Core.t) =
    try
      on_init core;
      (* setup plugins *)
      Logs.info ~src:Core.logs_src (fun k -> k "creating plugins…");
      let plugins = Plugin.Set.create ?cmd_help ~io:core.io ~sw conf all in
      Logs.info ~src:Core.logs_src (fun k ->
          k "plugins initialized (ok: %b)…" (Result.is_ok plugins));
      let plugins = unwrap_result_failwith plugins in
      plugins_ref := Some plugins;
      (* wait a bit before joining *)
      core.io.sleep 2.;
      Logs.info ~src:Core.logs_src (fun k -> k "joining channels…");
      List.iter (fun c -> Core.send_join core ~channel:c) conf.Config.channels;
      Logs.info ~src:Core.logs_src (fun k ->
          k "run %d plugins" (List.length all))
    with exn ->
      let msg = Printexc.to_string exn in
      Logs.err ~src:Core.logs_src (fun k -> k "error in init: %s" msg)
  in
  let callback (core : Core.t) msg =
    match !plugins_ref with
    | None -> ()
    | Some plugins ->
      let cmds = Plugin.Set.commands plugins in
      let on_msg_l = Plugin.Set.on_msg_l plugins in
      List.iter (fun f -> f core msg) on_msg_l;
      (match Core.privmsg_of_msg msg with
      | None -> ()
      | Some msg ->
        let prefix = conf.Config.prefix in
        Command.run ~prefix core cmds msg)
  in
  Core.run ~sw ~net ~clock conf ~on_connect ~callback ()
