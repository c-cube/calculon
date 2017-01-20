(** {1 Plugins} *)

open Lwt.Infix

type stateful = St : 'st stateful_ -> stateful
and 'st stateful_ = {
  commands: 'st -> Command.t list;
  init: Core.t -> Config.t -> 'st Lwt.t;
  stop: 'st -> unit Lwt.t;
}

(** A single plugin *)
type t =
  | Stateful of stateful
  | Stateless of Command.t list

let stateful ~init ~stop commands =
  Stateful (St { init; stop; commands; })

let of_cmd c = Stateless [c]
let of_cmds l = Stateless l

let init core conf l =
  let init_st (St st) =
    st.init core conf >>= fun state ->
    let cs = st.commands state in
    let cleanup() = st.stop state in
    Lwt.return (cs, cleanup)
  in
  Lwt_list.fold_left_s
    (fun (cmds,cleans) p -> match p with
       | Stateless cs -> Lwt.return (cs @ cmds, cleans)
       | Stateful st ->
         init_st st >|= fun (cs, cleanup) ->
         cs @ cmds, cleanup :: cleans)
    ([],[]) l
  >>= fun (cmds, cleans) ->
  let clean() =
    Lwt_list.iter_p (fun s->s()) cleans
  in
  Lwt.return (List.sort Command.compare_prio cmds, clean)

