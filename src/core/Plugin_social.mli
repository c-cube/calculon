
(** Social: keeps a register "nick -> informations" up-to-date.
    The type {!contact} can be extended to store new informations.

    One must then extend the function {!contacts_of_json}
    and {!json_of_contact} to handle the new fields (and deal with the
    case where we import old JSON values that don't have the new
    fields, e.g. by using a default value).

    The data stored in the contacts base are usually automatically updated
    by callbacks defined in {!Social} (thanks to {!Signal.on}).
*)

type to_tell = {
  from: string;
  on_channel: string;
  msg: string;
  tell_after: float option; (** optional; not before this deadline (UTC) *)
}

(* Data for contacts *)
type contact = {
  last_seen: float;
  to_tell: to_tell list;
  coucous : int;
}

type t

type state = t ref

val data : state -> string -> contact

(* by default, [force_sync] is true. Setting data with [force_sync] as false may
   result in data loss in case of reload/crash of the bot *)
val set_data : state -> ?force_sync:bool -> string -> contact -> unit

(* Sync the in-memory db with the on-disk storage *)
val sync : state -> unit

val plugin : Plugin.t

(**/**)
val is_coucou : string -> bool
(**/**)
