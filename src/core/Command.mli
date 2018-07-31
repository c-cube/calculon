(** {1 Command Type}

    A command is a particular rule for reacting to what is said on IRC.
    Typically, a command takes a {!Core.t} (to be able to answer
    and perform IRC actions) and an input message, and it decides
    whether to do something or not based on the message.

    The command returns a {!res}, which specifies whether it successfully
    "caught" the message (no need then for other commands to run),
    if it didn't react ("skip") so we can try the other commands,
    or whether it failed when trying to answer.
*)

type res =
  | Cmd_match of unit Lwt.t
  (** command applies, and fired with given action *)
  | Cmd_skip
  (** the command did not apply *)
  | Cmd_fail of string
  (** command applies, but failed *)

(** A command, bundling some metadata (name + descr, used for "!help"),
    a priority (used to run some commands before the others),
    and the answering function itself *)
type t = {
  prio: int; (* priority. The lower, the more urgent this command is. *)
  match_: Core.t -> Core.privmsg -> res; (* how to react to incoming messages *)
  name: string; (* name of the command *)
  descr: string; (* for !help *)
}

val match_prefix1 : prefix:string -> cmd:string -> Core.privmsg -> string option
(** [match_prefix1 ~prefix:"foo" msg]

    - if [msg="!foo bar"], returns [Some bar]
    - if [msg="!something else"], returns [None]
  *)

val extract_hl : string -> (string * string) option
(** [extract_hl "foo > bar"] returns [Some ("foo", "bar")].
    Returns [None] if it cannot split on ">" cleanly. *)

val match_prefix1_full : prefix:string -> cmd:string -> Core.privmsg -> (string * string option) option
(* @returns [Some (msg, hl)] if [msg] matches the regex,
   and [hl] is either [Some foo] if the message ended with "> hl",
   [None] otherwise *)

val make :
  ?descr:string ->
  ?prio:int ->
  name:string ->
  (Core.t -> Core.privmsg -> res) ->
  t
(** Make a command using the given methods. Only the name and
    matching rules are requested *)

exception Fail of string

val make_simple :
  ?descr:string ->
  ?prio:int ->
  ?prefix:string ->
  cmd:string ->
  (Core.privmsg -> string -> string option Lwt.t) ->
  t
(** [make_simple ~pre f] matches messages of the form "!pre xxx",
    and call [f msg "xxx"]. The function returns 0 or 1 line to reply to sender.
    The function can raise Fail to indicate failure *)

val make_simple_l :
  ?descr:string ->
  ?prio:int ->
  ?prefix:string ->
  cmd:string ->
  (Core.privmsg -> string -> string list Lwt.t) ->
  t
(** Same as {!make_simple} but replies lines
    The function can raise Fail to indicate failure *)

val make_simple_query_l :
  ?descr:string ->
  ?prio:int ->
  ?prefix:string ->
  cmd:string ->
  (Core.privmsg -> string -> string list Lwt.t) ->
  t
(** Same as {!make_simple_l} but replies lines in query (private)
    The function can raise Fail to indicate failure *)

val compare_prio : t -> t -> int
(** Compare by priority. Used to sort a list of commands by their priority. *)

val run : Core.t -> t list -> Core.privmsg -> unit Lwt.t
(** Execute the commands, in given order, on the message. First command
    to succeed shortcuts the other ones. *)
