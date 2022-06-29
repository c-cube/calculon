module C = Calculon

let plugins : C.Plugin.t list =
  [
    C.Plugin_social.plugin;
    C.Plugin_factoids.plugin;
    C.Plugin_state.plugin;
    C.Plugin_history.plugin ~n:40 ();
  ]

let config =
  {
    C.Config.default with
    C.Config.server = "irc.libera.chat";
    port = 6697;
    username = "test_bot";
    realname = "test_bot";
    nick = "test_bot";
    log_level = Logs.Info;
    tls = true;
    channels = [ "##test1234" ];
  }

let () =
  Logs.set_reporter (Logs.format_reporter ~dst:Format.err_formatter ());
  try
    (* update with CLI parameters *)
    let config = C.Config.parse config Sys.argv in
    Logs.set_level ~all:true (Some config.C.Config.log_level);
    C.Run_main.main config plugins |> Lwt_main.run
  with
  | Arg.Help msg -> print_endline msg
  | Arg.Bad msg ->
    prerr_endline msg;
    exit 1
