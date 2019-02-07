open Prelude

type key = private string (* utf8 *)
type value =
  | StrList of string list
  | Int of int
type factoid = {key: key; value: value}
type t = factoid StrMap.t

val key_of_string : string -> key option

val factoids_of_json : Yojson.Safe.json -> (t, string) CCResult.t
val json_of_factoids : t -> Yojson.Safe.json

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

val empty : t

val mem : key -> t -> bool
val get : key -> t -> value
val set : factoid -> t -> t
val append : factoid -> t -> t
val remove : factoid -> t -> t
val incr : key -> t -> int option * t
val decr : key -> t -> int option * t

val search : string list -> t -> string list

val plugin : Plugin.t
