(** Social: keeps a register "nick -> informations" up-to-date. The type
    {!contact} can be extended to store new informations.

    One must then extend the function {!contacts_of_json} and {!json_of_contact}
    to handle the new fields (and deal with the case where we import old JSON
    values that don't have the new fields, e.g. by using a default value).

    The data stored in the contacts base are usually automatically updated by
    callbacks defined in {!Social} (thanks to {!Signal.on}). *)

type to_tell = {
  from: string;
  on_channel: string;
  msg: string;
  tell_after: float option;  (** optional; not before this deadline (UTC) *)
}

(* Data for contacts *)
type contact = { last_seen: float; to_tell: to_tell list; ignore_user: bool }

val plugin : Plugin.t
