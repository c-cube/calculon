
(** {1 History plugin} *)

(** Keep the last [n] lines of history on a chan, and give them to newcomers
    that ask for them *)

val plugin : ?n:int -> unit -> Plugin.t
(** @param n the number of lines to retain *)
