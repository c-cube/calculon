
(** {1 Manage state} *)

type t = {
  actions: Plugin.action_callback;
}

let cmd_reload st =
  let open Lwt.Infix in
  Command.make_simple
    ~descr:"reload state from disk"
    ~prio:10
    ~prefix:"reload"
    (fun _ _ ->
       Signal.Send_ref.send st.actions Plugin.Require_reload >|= fun () ->
       Some (Talk.select Talk.Ack)
    )

let cmd_save st =
  let open Lwt.Infix in
  Command.make_simple
    ~descr:"save state to disk"
    ~prio:10
    ~prefix:"save"
    (fun _ _ ->
       Signal.Send_ref.send st.actions Plugin.Require_save >|= fun () ->
       Some (Talk.select Talk.Ack)
    )

let plugin =
  Plugin.stateful
    ~name:"state"
    ~of_json:(fun actions _ -> Lwt_err.return {actions})
    ~to_json:(fun _ -> None)
    ~stop:(fun _ -> Lwt.return_unit)
    ~commands:(fun st -> [ cmd_reload st; cmd_save st; ])
    ()
