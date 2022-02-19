open Prelude

type t = {
  server : string;
  port : int;
  username : string;
  password : string option;
  realname : string;
  nick : string;
  tls: bool;
  sasl: bool;
  channel : string;
  state_file : string;
  log_level: Logs.level;
  prefix: string; (** prefix for commands *)
}

let default = {
  server = "irc.libera.chat";
  port = 7000;
  username = "calculon";
  password = None;
  realname = "calculon";
  nick = "calculon";
  tls = true;
  sasl = true;
  channel = "#ocaml";
  state_file = "state.json";
  log_level=Logs.Info;
  prefix = "!";
}

let parse ?(extra_args=[]) conf args =
  let custom_nick = ref None in
  let custom_chan = ref None in
  let custom_server = ref None in
  let custom_state = ref None in
  let custom_port = ref conf.port in
  let custom_tls = ref None in
  let custom_sasl = ref None in
  let prefix = ref default.prefix in
  let log_lvl = ref None in
  let options = Arg.align @@ extra_args @
      [ "--nick", Arg.String (fun s -> custom_nick := Some s),
        " custom nickname (default: " ^ default.nick ^ ")"
      ; "--chan", Arg.String (fun s -> custom_chan := Some s),
        " channel to join (default: " ^ default.channel ^ ")"
      ; "--port", Arg.Set_int custom_port, " port of the server"
      ; "--server", Arg.String (fun s -> custom_server := Some s),
        " server to join (default: " ^ default.server ^ ")"
      ; "--state", Arg.String (fun s -> custom_state := Some s),
        " file containing factoids (default: " ^ default.state_file ^ ")"
      ; "--tls", Arg.Bool (fun b -> custom_tls := Some b), " enable/disable TLS"
      ; "--sasl", Arg.Bool (fun b -> custom_sasl := Some b), " enable/disable SASL auth"
      ; "--debug", Arg.Unit (fun() ->log_lvl := Some Logs.Debug), " set log level to debug"
      ; "--prefix", Arg.Set_string prefix, " set prefix for commands (default \"!\")";
      ]
  in
  Arg.parse_argv args options ignore "parse options";

  (* env vars are also used *)
  let user =
    try Sys.getenv "USER" with _ -> conf.username
  and password =
      try Some (Sys.getenv "PASSWORD") with _ -> None
  in

  { conf with
    nick = !custom_nick |? conf.nick;
    username = user;
    password;
    channel = !custom_chan |? conf.channel;
    server = !custom_server |? conf.server;
    tls = !custom_tls |? conf.tls;
    sasl = !custom_sasl |? conf.sasl;
    port = !custom_port;
    state_file = !custom_state |? conf.state_file;
    log_level = !log_lvl |? conf.log_level;
    prefix = !prefix;
  }

let of_argv ?extra_args () =
  try parse ?extra_args default Sys.argv
  with
    | Arg.Bad msg -> print_endline msg; exit 1
    | Arg.Help msg -> print_endline msg; exit 0
