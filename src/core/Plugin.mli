(** {1 Plugins}

    A plugin is a bunch of commands, and optionally some disk-backed state.
    It will register its commands to the core loop *)

type json = Yojson.Safe.json

type action =
  | Require_reload
  (** Require that we reload everything from on-disk state *)
  | Require_save
  (** Require that the state be saved *)

type action_callback = action Signal.Send_ref.t

(** A stateful plugin, using a persistent state ['st] *)
type stateful = St : 'st stateful_ -> stateful

and 'st stateful_ = private {
  name: string;
  (* namespace for storing state. Must be distinct for every plugin. *)
  commands: 'st -> Command.t list;
  (* commands parametrized by some (mutable) state, with the ability
     to trigger a signal *)
  on_msg:'st -> (Core.t -> Irc_message.t -> unit Lwt.t) list;
  (* executed on each incoming message *)
  to_json : 'st -> json option;
  (* how to serialize (part of) the state into JSON, if need be. *)
  of_json : action_callback -> json option -> ('st, string) Result.result Lwt.t;
  (* how to deserialize the state. [None] is passed for a fresh
     initialization *)
  stop: 'st -> unit Lwt.t;
  (* stop the plugin.
     It is NOT the responsibility of this command to save the state,
     as the core engine will have called {!to_json} before. *)
}

(** A single plugin *)
type t =
  | Stateful of stateful
  | Stateless of Command.t list

type plugin = t

val of_cmd : ?prefix:string -> Command.t -> t
(** Stateless plugin with 1 command *)

val of_cmds : ?prefix:string -> Command.t list -> t
(** Stateless plugin with several commands
    @raise Invalid_argument if the list is empty *)

val stateful :
  name:string ->
  commands:('st -> Command.t list) ->
  ?on_msg:('st -> (Core.t -> Irc_message.t -> unit Lwt.t) list) ->
  to_json:('st -> json option) ->
  of_json:(action_callback -> json option -> ('st, string) Result.result Lwt.t) ->
  ?stop:('st -> unit Lwt.t) ->
  unit ->
  t
(** Make a stateful plugin using the given [name] (for prefixing
    its storage; this should be unique) and ways to serialize state to Json,
    deserialize state from Json, and building commands from the state.
    See {!stateful_} for more details on each field. *)

(** {2 Collection of Plugins} *)
module Set : sig
  type t

  val create :
    Config.t ->
    plugin list ->
    (t, string) Result.result Lwt.t
  (** Create a collection of plugins, loading the state, initializing
      them *)

  val commands : t -> Command.t list
  (** Corresponding list of commands *)

  val on_msg_l : t -> (Core.t -> Irc_message.t -> unit Lwt.t) list
  (** List of callbacks called on each message *)

  val save : t -> unit Lwt.t
  (** Save state to disk *)

  val reload : t -> (unit, string) Result.result Lwt.t
  (** Reload state from disk *)

  val stop : ?save:bool -> t -> unit Lwt.t
  (** Stop all plugins
      @param save if [true], will call {!save} first (default [true]) *)
end
