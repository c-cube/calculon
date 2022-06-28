(** {1 Basic signal} *)

type 'a t
(** Signal of type 'a *)

type 'a signal = 'a t

val create : unit -> 'a t
(** New signal *)

val send : 'a t -> 'a -> unit Lwt.t
(** Trigger the signal *)

type handler_response = ContinueListening | StopListening

val on : 'a t -> ('a -> handler_response Lwt.t) -> unit
(** Register a handler to the signal; the handler returns [ContinueListening]
      if it wants to continue being notified, [StopListening] otherwise *)

val on' : 'a t -> ('a -> 'b Lwt.t) -> unit

val once : 'a t -> ('a -> 'b Lwt.t) -> unit
(** Register a handler to be called only once *)

val propagate : 'a t -> 'a t -> unit
(** [propagate a b] propagates all values of [a] into [b]. Cycles
      are not detected. *)

(** {2 Combinators} *)

val map : 'a t -> ('a -> 'b) -> 'b t
val filter : 'a t -> ('a -> bool) -> 'a t
val filter_map : 'a t -> ('a -> 'b option) -> 'b t

val set_exn_handler : (exn -> unit) -> unit
(** Set the handler that is called upon an exception in
      a Signal.  The default handler does nothing.
      If the handler raises an exception, it is not caught! *)

(** {2 Send-only View} *)

(** Can be used only for sending *)

module Send_ref : sig
  type 'a t

  val make : 'a signal -> 'a t
  val send : 'a t -> 'a -> unit Lwt.t
end
