
(** {1 Small Parser for IRC Logs} *)

type 'a sequence = ('a -> unit) -> unit

(** One message in a IRC log *)
type log_record = {
  author: string;
  time: string;
  msg: string;
}

val re_irssi : Re.re
(** Irssi logs *)

val re_weechat : Re.re
(** Weechat logs *)

type fmt =
  | Irssi
  | Weechat

val parse_record : fmt -> string -> log_record option
(** Parse one line of log *)

val string_of_record : log_record -> string
(** Print record *)

val pp_record : log_record CCFormat.printer

val iter_file : fmt -> string -> log_record sequence

val iter_dir : fmt -> string -> (string * log_record) sequence

val norm_author : string -> string
(** Normalize author's name *)
