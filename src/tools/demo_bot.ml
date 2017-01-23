module C = Calculon
module CW = Calculon_web

let plugins : C.Plugin.t list = [
  C.Plugin_social.plugin;
  C.Plugin_factoids.plugin;
  CW.Plugin_web.plugin;
]

let config = {
  C.Config.server = "127.0.0.1";
  port = 6697;
  username = "botbot";
  realname = "botbot";
  nick = "botbot";
  channel = "#test";
  state_file = "state.json"
}

let () =
  C.Run_main.main config plugins |> Lwt_main.run
