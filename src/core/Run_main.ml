
open Lwt.Infix

let main conf all : unit Lwt.t =
  let init (core:Core.t) =
    let (module C) = core in
    (* setup plugins *)
    Plugin.init core conf all >>= fun (cmds, cleanup_cmds) ->
    (* connect to chan *)
    C.send_join ~channel:conf.Config.channel >>= fun () ->
    Log.logf "got %d commands from %d plugins" (List.length cmds) (List.length all);
    (* log incoming messages, apply commands to them *)
    Signal.on' C.messages
      (fun msg ->
         Log.logf "got message: %s" (Core.Msg.to_string msg);
         match Core.privmsg_of_msg msg with
           | None -> Lwt.return_unit
           | Some msg -> Command.run core cmds msg);
    (* cleanup *)
    Lwt.async (fun () -> C.exit >>= cleanup_cmds);
    Lwt.return_unit
  and connect =
    Core.connect_of_config conf
  in
  Core.run
    ~connect
    ~init
    ()
