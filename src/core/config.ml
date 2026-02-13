open Prelude

type hidden = Hidden

type t = {
  server: string;
  port: int;
  username: string;
  password: string option;
  realname: string;
  nick: string;
  tls: bool;
  sasl: bool;
  channels: string list;
  log_level: Logs.level;
  prefix: string;
  db_file: string;
  _hidden: hidden;
}

let default =
  {
    server = "irc.libera.chat";
    port = 7000;
    username = "calculon";
    password = None;
    realname = "calculon";
    nick = "calculon";
    tls = true;
    sasl = true;
    channels = [ "#ocaml" ];
    log_level = Logs.Info;
    prefix = "!";
    db_file = "calculon.db";
    _hidden = Hidden;
  }

let parse ?(extra_args = []) conf args =
  let custom_nick = ref None in
  let custom_chan = ref [] in
  let custom_server = ref None in
  let custom_db_file = ref None in
  let custom_port = ref conf.port in
  let custom_tls = ref None in
  let custom_sasl = ref None in
  let prefix = ref default.prefix in
  let log_lvl = ref None in
  let options =
    Arg.align @@ extra_args
    @ [
        ( "--nick",
          Arg.String (fun s -> custom_nick := Some s),
          " custom nickname (default: " ^ default.nick ^ ")" );
        ( "--chan",
          Arg.String (fun s -> custom_chan := s :: !custom_chan),
          " channel to join (default: "
          ^ String.concat "," default.channels
          ^ ")" );
        "--port", Arg.Set_int custom_port, " port of the server";
        ( "--server",
          Arg.String (fun s -> custom_server := Some s),
          " server to join (default: " ^ default.server ^ ")" );
        ( "--db-file",
          Arg.String (fun s -> custom_db_file := Some s),
          " database file containing plugin state (default: " ^ default.db_file
          ^ ")" );
        "--tls", Arg.Bool (fun b -> custom_tls := Some b), " enable/disable TLS";
        ( "--sasl",
          Arg.Bool (fun b -> custom_sasl := Some b),
          " enable/disable SASL auth" );
        ( "--debug",
          Arg.Unit (fun () -> log_lvl := Some Logs.Debug),
          " set log level to debug" );
        ( "--prefix",
          Arg.Set_string prefix,
          " set prefix for commands (default \"!\")" );
      ]
  in
  Arg.parse_argv args options ignore "parse options";

  (* env vars are also used *)
  let user = try Sys.getenv "USER" with _ -> conf.username
  and password = try Some (Sys.getenv "PASSWORD") with _ -> None in

  {
    conf with
    nick = !custom_nick |? conf.nick;
    username = user;
    password;
    channels =
      (if !custom_chan = [] then
         conf.channels
       else
         !custom_chan);
    server = !custom_server |? conf.server;
    tls = !custom_tls |? conf.tls;
    sasl = !custom_sasl |? conf.sasl;
    port = !custom_port;
    db_file = !custom_db_file |? conf.db_file;
    log_level = !log_lvl |? conf.log_level;
    prefix = !prefix;
  }

let of_argv ?extra_args () =
  try parse ?extra_args default Sys.argv with
  | Arg.Bad msg ->
    print_endline msg;
    exit 1
  | Arg.Help msg ->
    print_endline msg;
    exit 0
