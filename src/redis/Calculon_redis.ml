
open Lwt.Infix
module R = Redis_lwt.Client
open Calculon_redis_lib

type config = {
  prefix: string;
  host: string;
  port: int;
}

type t = {
  config: config;
  sub: R.connection;
  pub: R.connection;
  mutable core: Core.t option;
  mutable fwd: unit Lwt.t; (* forwarding from sub to IRC *)
}

let fwd_loop (self:t) : unit Lwt.t =
  R.subscribe self.sub [self.config.prefix ^ ":send"] >>= fun () ->
  let process_send s =
    Logs.debug ~src:Core.logs_src (fun k->k "redis.fwd-loop: process %S" s);
    match self.core, Message_j.send_of_string s with
    | Some (module C), send ->
      begin match send.message with
        | `Privmsg {Message_t.dest; source=_; message} ->
          Logs.debug ~src:Core.logs_src (fun k->k "redis.fwd-loop: send-privmsg %s" s);
          C.send_privmsg ~target:dest ~message
        | `Join l ->
          Logs.debug ~src:Core.logs_src (fun k->k "redis.fwd-loop: join %s" (String.concat "," l));
          Lwt_list.iter_s (fun c -> C.send_join ~channel:c) l
        | `Part l ->
          Logs.debug ~src:Core.logs_src (fun k->k "redis.fwd-loop: join %s" (String.concat "," l));
          Lwt_list.iter_s (fun c -> C.send_part ~channel:c) l
      end
    | None, _ -> Lwt.return () (* cannot send *)
    | exception _ -> Lwt.return ()
  in
  (* process messages *)
  Lwt_stream.iter_s
    (fun (l:R.reply list) ->
       Lwt_list.iter_s
         (function
           | `Bulk (Some s) -> process_send s
           | _ -> Lwt.return())
         l)
    (R.stream self.sub)

let start (config:config) : (t,_) result Lwt.t =
  Lwt.catch
    (fun () ->
       let {host;port;_} = config in
       let spec = {R.host;port} in
       Logs.info ~src:Core.logs_src (fun k->k "connecting to redis on %s:%d..." host port);
       R.connect spec >>= fun pub ->
       R.connect spec >>= fun sub ->
       Logs.info ~src:Core.logs_src (fun k->k "connected to redis on %s:%d" host port);
       let st = {config; pub; sub; core=None; fwd=Lwt.return ()} in
       st.fwd <- fwd_loop st;
       Lwt.return (Ok st))
    (fun e ->
       Lwt.return (Error ("redis plugin: " ^ Printexc.to_string e)))

let stop (self:t) =
  Lwt.cancel self.fwd;
  R.disconnect self.sub >>= fun () ->
  R.disconnect self.pub

(* process received messages *)
let mk_on_msg (self:t) (core:Calculon.Core.t) (msg:Irc_message.t) : unit Lwt.t =
  self.core <- Some core; (* update *)
  let m = match msg.command with
    | Irc_message.PRIVMSG _ ->
      begin match Core.privmsg_of_msg msg with
        | None -> assert false
        | Some {Core. nick; to_; message } ->
          Some (`Privmsg {Message_t.dest=to_; source=nick; message})
      end
    | Irc_message.JOIN (l, _) -> Some (`Join l)
    | Irc_message.PART (l, _) -> Some (`Part l)
    | _ -> None
  in
  match m with
  | None -> Lwt.return ()
  | Some m ->
    let j = Message_j.string_of_message m in
    R.publish self.pub (self.config.prefix ^ ":received") j
    >|= fun (_:int) -> ()

let make_plugin ?(prefix="irc") ?(host="127.0.0.1") ?(port=6379) () : Plugin.t =
  let config = {prefix; port; host} in
  Plugin.stateful ~name:"redis"
    ~commands:(fun _ -> []) ~stop
    ~on_msg:(fun st -> [mk_on_msg st])
    ~to_json:(fun _ -> None)
    ~of_json:(fun _ _ -> start config)
    ()
