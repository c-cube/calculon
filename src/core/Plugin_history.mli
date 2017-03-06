
(** {1 History plugin} *)

(** Keep the last [n] lines of history on a chan, and give them to newcomers
    that ask for them *)

val plugin : ?default_len:int -> ?n:int -> unit -> Plugin.t
(** plugin for storing [n] lines of history and giving them back in query
    to people who ask.
    @param n the number of lines to retain internally
    @param default_len the number of lines to display by default, when
      replying to message "!history" *)
