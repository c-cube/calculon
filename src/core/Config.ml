open Prelude

type irc_log =
  [ `None
  | `Chan of Lwt_io.output_channel
  | `Custom of (string -> unit Lwt.t)
  ]

type t = {
  server : string;
  port : int;
  username : string;
  realname : string;
  nick : string;
  tls: bool;
  channel : string;
  state_file : string;
  irc_log: irc_log; (* log IRC events *)
}

let default = {
  server = "irc.freenode.net";
  port = 7000;
  username = "calculon";
  realname = "calculon";
  nick = "calculon";
  tls = true;
  channel = "#ocaml";
  state_file = "state.json";
  irc_log = `None;
}

let parse conf args =
  let custom_nick = ref None in
  let custom_chan = ref None in
  let custom_server = ref None in
  let custom_state = ref None in
  let custom_port = ref 7000 in
  let custom_tls = ref None in
  let debug_stderr = ref false in
  let options = Arg.align
      [ "--nick", Arg.String (fun s -> custom_nick := Some s),
        " custom nickname (default: " ^ default.nick ^ ")"
      ; "--chan", Arg.String (fun s -> custom_chan := Some s),
        " channel to join (default: " ^ default.channel ^ ")"
      ; "--port", Arg.Set_int custom_port, " port of the server"
      ; "--server", Arg.String (fun s -> custom_server := Some s),
        " server to join (default: " ^ default.server ^ ")"
      ; "--state", Arg.String (fun s -> custom_state := Some s),
        " file containing factoids (default: " ^ default.state_file ^ ")"
      ; "--tls", Arg.Unit (fun () -> custom_tls := Some true), " enable TLS"
      ; "--no-tls", Arg.Unit (fun () -> custom_tls := Some false), " disable TLS"
      ; "--debug", Arg.Set debug_stderr, " print IRC debug messages on stderr"
      ]
  in
  Arg.parse_argv args options ignore "parse options";
  { conf with
    nick = !custom_nick |? conf.nick;
    channel = !custom_chan |? conf.channel;
    server = !custom_server |? conf.server;
    tls = !custom_tls |? conf.tls;
    port = !custom_port;
    state_file = !custom_state |? conf.state_file;
    irc_log = (if !debug_stderr then `Chan Lwt_io.stderr else `None);
  }

let of_argv () =
  try parse default Sys.argv
  with
    | Arg.Bad msg -> print_endline msg; exit 1
    | Arg.Help msg -> print_endline msg; exit 0
