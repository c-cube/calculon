open Prelude

type key = private string (* utf8 *)
type value =
  | StrList of string list
  | Int of int
type factoid = {key: key; value: value}
type t = factoid StrMap.t

val key_of_string : string -> key option

type op =
  | Get of key
  | Set of factoid
  | Set_force of factoid
  | Append of factoid
  | Incr of key
  | Decr of key

val parse_op : string -> (op * string option) option
(** op + hilight *)

val string_of_value : value -> string
val string_of_op : op -> string

val empty : t

val mem : key -> t -> bool
val get : key -> t -> value
val set : factoid -> t -> t
val append : factoid -> t -> t
val incr : key -> t -> int option * t
val decr : key -> t -> int option * t

val search : string list -> t -> string list

val read_file : file:string -> t Lwt.t
val write_file : file:string -> t -> unit Lwt.t

val plugin : Plugin.t
