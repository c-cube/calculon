(** {1 Plugins}

    A plugin is a bunch of commands, and optionally some disk-backed state. It
    will register its commands to the core loop *)

open Db_utils

type json = Yojson.Safe.t

type action =
  | Require_reload  (** Require that we reload everything from on-disk state *)
  | Require_save  (** Require that the state be saved *)

type action_callback = action Signal.Send_ref.t

(** A stateful plugin, using a persistent state ['st] *)
type stateful = St : 'st stateful_ -> stateful

and 'st stateful_ = private {
  name: string;
      (** Namespace for storing state. Must be distinct for every plugin. *)
  commands: 'st -> Command.t list;
      (** Commands parametrized by some (mutable) state, with the ability to
          trigger a signal *)
  on_msg: 'st -> (Core.t -> Irky.Message.t -> unit) list;
      (** Executed on each incoming message *)
  to_json: 'st -> json option;
      (** How to serialize (part of) the state into JSON, if need be. *)
  of_json: action_callback -> json option -> ('st, string) result;
      (** How to deserialize the state. [None] is passed for a fresh
          initialization. *)
  stop: 'st -> unit;
      (** Stop the plugin. It is NOT the responsibility of this command to save
          the state, as the core engine will have called {!to_json} before. *)
}

type db_backed = private {
  commands: DB.db -> Command.t list;
      (** Commands parametrized by some (mutable) state, with the ability to
          trigger a signal *)
  prepare_db: DB.db -> unit;
      (** Prepare database (create tables, etc.). Must be idempotent as it'll be
          called every time the plugin is initialized. *)
  on_msg: DB.db -> (Core.t -> Irky.Message.t -> unit) list;
      (** Executed on each incoming message *)
  stop: DB.db -> unit;
      (** Stop the plugin. There is no need to close the DB connection. *)
}

(** A single plugin *)
type t = private
  | Stateful of stateful
  | Stateless of Command.t list
  | DB_backed of db_backed

type plugin = t

val of_cmd : Command.t -> t
(** Stateless plugin with 1 command. *)

val of_cmds : Command.t list -> t
(** Stateless plugin with several commands.
    @raise Invalid_argument if the list is empty *)

val stateful :
  name:string ->
  commands:('st -> Command.t list) ->
  ?on_msg:('st -> (Core.t -> Irky.Message.t -> unit) list) ->
  to_json:('st -> json option) ->
  of_json:(action_callback -> json option -> ('st, string) result) ->
  ?stop:('st -> unit) ->
  unit ->
  t
(** Make a stateful plugin using the given [name] (for prefixing its storage;
    this should be unique) and ways to serialize state to Json, deserialize
    state from Json, and building commands from the state. See {!stateful_} for
    more details on each field. *)

val db_backed :
  commands:(DB.db -> Command.t list) ->
  prepare_db:(DB.db -> unit) ->
  ?on_msg:(DB.db -> (Core.t -> Irky.Message.t -> unit) list) ->
  ?stop:(DB.db -> unit) ->
  unit ->
  t
(** Make a stateful plugin that is backed by some tables in the database. See
    {!db_backed} for more details.
    @since 0.8 *)

(** {2 Collection of Plugins} *)
module Set : sig
  type t

  val create :
    ?cmd_help:bool ->
    io:Irky.Io.t ->
    sw:Eio.Switch.t ->
    Config.t ->
    plugin list ->
    (t, string) result
  (** Create a collection of plugins, loading the state, initializing them.
      @param cmd_help if true, adds a "help" command. *)

  val commands : t -> Command.t list
  (** Corresponding list of commands *)

  val on_msg_l : t -> (Core.t -> Irky.Message.t -> unit) list
  (** List of callbacks called on each message *)

  val save : t -> unit
  (** Save state to disk *)

  val reload : t -> (unit, string) result
  (** Reload state from disk *)

  val stop : ?save:bool -> t -> unit
  (** Stop all plugins
      @param save if [true], will call {!save} first (default [true]) *)
end
