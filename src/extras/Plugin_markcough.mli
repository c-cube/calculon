
(** {1 Markov Chains} *)

(** {2 Transition Table} *)
module Table : sig
  type token =
    | Start
    | Stop
    | Word of string (* non empty string *)

  val print_token : token CCFormat.printer

  (** A prefix tree of uniform length *)
  type t

  val empty : t

  val singleton : token -> t

  val add : token list -> token -> t -> t

  val merge : t -> t -> t

  val merge_list : t list -> t

  val pick_key : Random.State.t -> t -> token
  (** [pick_key rand t] picks one of the keys of [t]
      @raise Not_found if [t] is empty *)

  val pick : Random.State.t -> token list -> t -> token
  (** Pick a token following the given path, randomly
      @raise Not_found if there is no token to be found *)

  val print : t CCFormat.printer
  (** Pretty-print the tree on the given formatter *)

  val write_to : out_channel -> t -> unit
  (** Serialize to channel *)

  val read_from : in_channel -> t
  (** Deserialize from channel *)
end

(** {2 Parse IRC logs into a table} *)
module Parse_logs : sig
  val parse_file : Irclog.fmt -> string -> Table.t -> Table.t

  val parse_dir : Irclog.fmt -> string -> Table.t -> Table.t

  val parse_file_or_dir : Irclog.fmt -> string -> Table.t -> Table.t
end

(** {2 Generate} *)
module Gen : sig
  val generate : ?author:string -> ?rand:Random.State.t -> Table.t -> string
end

val plugin : Calculon.Plugin.t
