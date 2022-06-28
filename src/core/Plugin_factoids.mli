type key = private string (* utf8 *)

type value = StrList of string list | Int of int
type factoid = { key: key; value: value }
type t

val key_of_string : string -> key option

type op =
  | Get of key
  | Set of factoid
  | Set_force of factoid
  | Append of factoid
  | Remove of factoid
  | Incr of key
  | Decr of key

val parse_op : prefix:string -> string -> (op * string option) option
(** op + hilight *)

val string_of_value : value -> string
val string_of_op : op -> string
val plugin : Plugin.t

val set_max_cardinal_for_force : int -> unit
(** [set_max_cardinal_for_force n] prevents [Set_force] for keys
    with more than [n] factoids, to not lose too much data. *)
