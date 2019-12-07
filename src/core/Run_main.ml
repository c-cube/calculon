
let main ?cmd_help conf all : unit Lwt.t =
  let init_or_err (core:Core.t): unit Lwt_err.t =
    let open Lwt_err in
    let (module C) = core in
    (* setup plugins *)
    Plugin.Set.create ?cmd_help conf all >>= fun plugins ->
    (* connect to chan *)
    C.send_join ~channel:conf.Config.channel |> ok >|= fun () ->
    Logs.info ~src:Core.logs_src (fun k->k "run %d plugins" (List.length all));
    (* log incoming messages, apply commands to them *)
    let prefix = conf.Config.prefix in
    Signal.on' C.messages
      (fun msg ->
         let cmds = Plugin.Set.commands plugins in
         let on_msg_l = Plugin.Set.on_msg_l plugins in
         Logs.info ~src:Core.logs_src
           (fun k->k "got message: %s" (Irc_message.to_string msg));
         let open Lwt.Infix in
         Lwt_list.iter_s (fun f -> f core msg) on_msg_l >>= fun () ->
         match Core.privmsg_of_msg msg with
           | None -> Lwt.return_unit
           | Some msg -> Command.run ~prefix core cmds msg);
    ()
  in
  (* error-logging wraper *)
  let init core : unit Lwt.t =
    let open Lwt.Infix in
    init_or_err core >|= function
    | Result.Ok () -> ()
    | Result.Error msg ->
      Logs.err ~src:Core.logs_src (fun k->k "error in main loop: %s" msg);
  in
  Core.run conf ~init ()
