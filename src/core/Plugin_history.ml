(** {1 History plugin} *)

(** Keep the last [n] lines of history on a chan, and give them to newcomers
    that ask for them *)

(* time, nick, msg *)
type line = { time: float; nick_: string; msg: string }

type t = {
  actions: Plugin.action_callback;
  hist: line Queue.t;
  size: int; (* max size of [hist] *)
  default_len: int; (* default length of history in query *)
}

let on_msg state _ m =
  match Core.privmsg_of_msg m with
  | None -> Lwt.return ()
  | Some { Core.to_; _ } when not (Core.is_chan to_) ->
    Lwt.return () (* ignore private messages *)
  | Some { Core.nick; to_ = _; message } ->
    let time = Unix.gettimeofday () in
    let line = { time; nick_ = nick; msg = message } in
    (* pop oldest item if the queue is full *)
    if Queue.length state.hist >= state.size then ignore (Queue.pop state.hist);
    Queue.push line state.hist;
    Lwt.return ()

(* list of lines in history *)
let reply_history state n : string list =
  assert (n > 0);
  Queue.fold
    (fun acc line ->
      let time =
        Ptime.of_float_s line.time |> Option.get_or "invalid timestamp"
      in
      let line' =
        Printf.sprintf "[%s] %s: %s"
          (Ptime.to_rfc3339 ~space:false time)
          line.nick_ line.msg
      in
      line' :: acc)
    [] state.hist
  |> CCList.take n (* take the last [n] messages *)
  |> List.rev

let cmd_history st : _ =
  Command.make_simple_query_l
    ~descr:
      (Printf.sprintf "give back <n> lines of history in query (max %d)" st.size)
    ~prio:10 ~cmd:"history" (fun _ msg ->
      let msg = String.trim msg in
      if msg = "" then
        Lwt.return @@ reply_history st st.default_len
      else
        Lwt.return
          (* parse the number of lines *)
          (try
             let n = int_of_string msg in
             if n > 0 then
               reply_history st n
             else
               [ Talk.select Talk.Err ]
           with _ -> [ Talk.select Talk.Err ]))

let plugin ?(default_len = 10) ?(n = 150) () =
  Plugin.stateful ~name:"history"
    ~of_json:(fun actions _ ->
      Ok { actions; size = n; default_len; hist = Queue.create () })
    ~to_json:(fun _ -> None)
    ~on_msg:(fun state -> [ on_msg state ])
    ~stop:(fun _ -> Lwt.return ())
    ~commands:(fun st -> [ cmd_history st ])
    ()
