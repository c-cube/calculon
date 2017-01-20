(** {1 Plugins}

    A plugin is a bunch of commands, and optionally some disk-backed state.
    It will register its commands to the core loop *)

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

val of_cmd : Command.t -> t

val of_cmds : Command.t list -> t

val stateful :
  init:(Core.t -> Config.t -> 'st Lwt.t) ->
  stop:('st -> unit Lwt.t) ->
  ('st -> Command.t list) ->
  t

val init : Core.t -> Config.t -> t list -> (Command.t list * (unit -> unit Lwt.t)) Lwt.t
(** [init core conf plugins] initializes each plugin with the config,
    and returns the (sorted) list of commands along with
    a cleanup function *)
