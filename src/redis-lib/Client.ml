
open Lwt.Infix
module R = Redis_lwt.Client

type config = {
  debug: bool;
  prefix: string;
  host: string;
  port: int;
}

type t = {
  config: config;
  send: R.connection;
  received: R.connection;
  stream: Message_t.message Lwt_stream.t;
  stop: unit Lwt.u;
  join: unit Lwt.t;
}

let received self = self.stream
let join self = self.join

let on_received self f : unit =
  Lwt.async (fun () -> Lwt_stream.iter_p f self.stream)

let send (self:t) (m:Message_t.send) : unit Lwt.t =
  if self.config.debug then (
    Printf.eprintf "send msg %s\n%!" (Message_j.string_of_send m);
  );
  R.publish self.send (self.config.prefix ^ ":send") (Message_j.string_of_send m)
  >|= fun (_:int) -> ()

let get_msg (l:R.reply list) : _ list Lwt.t =
  Lwt_list.filter_map_p
    (function
      | `Bulk (Some s) ->
        begin match Message_j.message_of_string s with
          | m -> Lwt.return_some m
          | exception _ -> Lwt.return None
        end
      | _ -> Lwt.return None)
    l

let close (self:t) =
  Lwt.wakeup self.stop ();
  R.disconnect self.received >>= fun () ->
  R.disconnect self.send

let connect_ (config:config) : (t,_) result Lwt.t =
  Lwt.catch
    (fun () ->
       let {host;port;_} = config in
       let spec = {R.host;port} in
       if config.debug then Printf.eprintf "connecting to redis on %s:%d...\n%!" host port;
       let join, stop = Lwt.wait () in
       R.connect spec >>= fun send ->
       R.connect spec >>= fun received ->
       let str = R.stream received in
       Lwt.on_termination (Lwt_stream.closed str) (fun _ -> Lwt.wakeup stop ());
       R.subscribe received [config.prefix ^ ":received"] >>= fun () ->
       let stream = Lwt_stream.map_list_s get_msg str in
       if config.debug then (
         Printf.eprintf "connected to redis on %s:%d\n%!" host port;
       );
       let st = {config; send; received; join; stop; stream} in
       Lwt.return (Ok st))
    (fun e ->
       Lwt.return (Error ("redis lib : " ^ Printexc.to_string e)))

let make ?(debug=false) ?(prefix="irc") ?(host="127.0.0.1") ?(port=6379) () : (t,_) result Lwt.t =
  let config = {prefix; port; host; debug} in
  connect_ config
