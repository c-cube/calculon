(** {1 Command Type}

    A command is a particular rule for reacting to what is said on IRC. *)

type res =
  | Cmd_match of unit Lwt.t
  (** command applies, and fired with given action *)
  | Cmd_skip
  (** the command did not apply *)
  | Cmd_fail of string
  (** command applies, but failed *)

type t = {
  prio: int; (* priority. The lower, the more urgent this command is. *)
  match_: Core.t -> Core.privmsg -> res;
  name: string;
  descr: string; (* for !help *)
}

val match_prefix1 : prefix:string -> Core.privmsg -> string option
(** [match_prefix1 ~prefix:"foo" msg]

    - if [msg="!foo bar"], returns [Some bar]
    - if [msg="!something else"], returns [None]
  *)

val extract_hl : string -> (string * string) option
(** [extract_hl "foo > bar" returns [Some ("foo", "bar")].
    Returns [None] if it cannot split on ">" cleanly. *)

val match_prefix1_full : prefix:string -> Core.privmsg -> (string * string option) option
(* @returns [Some (msg, hl)] if [msg] matches the regex,
   and [hl] is either [Some foo] if the message ended with "> hl",
   [None] otherwise *)

val make :
  ?descr:string ->
  ?prio:int ->
  name:string ->
  (Core.t -> Core.privmsg -> res) ->
  t

exception Fail of string

val make_simple :
  ?descr:string ->
  ?prio:int ->
  prefix:string ->
  (Core.privmsg -> string -> string option Lwt.t) ->
  t
(** [make_simple ~pre f] matches messages of the form "!pre xxx",
    and call [f msg "xxx"]. The function returns 0 or 1 line to reply to sender.
    The function can raise Fail to indicate failure *)

val make_simple_l :
  ?descr:string ->
  ?prio:int ->
  prefix:string ->
  (Core.privmsg -> string -> string list Lwt.t) ->
  t
(** Same as {!make_simple} but replies lines
    The function can raise Fail to indicate failure *)

val make_simple_query_l :
  ?descr:string ->
  ?prio:int ->
  prefix:string ->
  (Core.privmsg -> string -> string list Lwt.t) ->
  t
(** Same as {!make_simple_l} but replies lines in query (private)
    The function can raise Fail to indicate failure *)

val compare_prio : t -> t -> int
(** Compare by priority *)

val run : Core.t -> t list -> Core.privmsg -> unit Lwt.t
(** Execute the commands, in given order, on the message. First command
    to succeed shortcuts the other ones. *)
