

module Config = struct
  type t = {
    check_certificate: bool;
    proto: Ssl.protocol;
  }

  let default = { check_certificate=false; proto=Ssl.TLSv1_3; }
end

module Io_ssl = struct
  type 'a t = 'a
  let[@inline] (>>=) x f = f x
  let[@inline] (>|=) x f = f x
  let[@inline] return x = x

  type file_descr = {
    ssl: Ssl.context;
    sslsock: Ssl.socket;
    fd: Unix.file_descr;
  }

  type config = Config.t
  type inet_addr = Unix.inet_addr

  let open_socket ?(config=Config.default) addr port : file_descr t =
    let ssl = Ssl.create_context config.Config.proto Ssl.Client_context in
    if config.Config.check_certificate then begin
      (* from https://github.com/johnelse/ocaml-irc-client/pull/21 *)
      Ssl.set_verify_depth ssl 3;
      Ssl.set_verify ssl [Ssl.Verify_peer] (Some Ssl.client_verify_callback);
      Ssl.set_client_verify_callback_verbose true;
    end;
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let sockaddr = Unix.ADDR_INET (addr, port) in
    (* Printf.printf "connect socket…\n%!"; *)
    Unix.connect sock sockaddr;
    (* Printf.printf "Ssl.connect socket…\n%!"; *)
    let sslsock = Ssl.embed_socket sock ssl in
    Ssl.connect sslsock;
    {fd=sock; ssl; sslsock;}

  let close_socket {fd;sslsock=_;ssl=_} =
    Unix.close fd

  let read self i len = Ssl.read self.sslsock i len
  let write self s i len = Ssl.write self.sslsock s i len

  let read_with_timeout ~timeout:_ self buf off len =
    try Some (Ssl.read self.sslsock buf off len)
    with Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> None

  let gethostbyname name =
    try
      let entry = Unix.gethostbyname name in
      let addrs = Array.to_list entry.Unix.h_addr_list in
      addrs
    with Not_found -> []

  let iter = List.iter
  let sleep d = Unix.sleepf (float d)
  let catch f err = try  f() with e -> err e
  let time = Unix.gettimeofday

  let pick = None
end

include Irc_client.Make(Io_ssl)
