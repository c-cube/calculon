(** {1 Manage state} *)

open Lwt_infix

type t = { actions: Plugin.action_callback }

let cmd_reload st =
  Command.make_simple ~descr:"reload state from disk" ~prio:10 ~cmd:"reload"
    (fun _ _ ->
      let+ () = Signal.Send_ref.send st.actions Plugin.Require_reload in
      Some (Talk.select Talk.Ack))

let cmd_save st =
  Command.make_simple ~descr:"save state to disk" ~prio:10 ~cmd:"save"
    (fun _ _ ->
      let+ () = Signal.Send_ref.send st.actions Plugin.Require_save in
      Some (Talk.select Talk.Ack))

let plugin =
  Plugin.stateful ~name:"state"
    ~of_json:(fun actions _ -> Ok { actions })
    ~to_json:(fun _ -> None)
    ~stop:(fun _ -> Lwt.return ())
    ~commands:(fun st -> [ cmd_reload st; cmd_save st ])
    ()
