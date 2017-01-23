(* Auto-generated from "movie.atd" *)


type year = Movie_t.year

type maybe = Movie_t.maybe

type search_entry = Movie_t.search_entry = { s_title: maybe; s_id: string }

type search_result = Movie_t.search_result = {
  count: int;
  results: search_entry list
}

type query_entry = Movie_t.query_entry = {
  title: maybe;
  id: string;
  year: year;
  rating: float;
  plot: maybe
}

val write_year :
  Bi_outbuf.t -> year -> unit
  (** Output a JSON value of type {!year}. *)

val string_of_year :
  ?len:int -> year -> string
  (** Serialize a value of type {!year}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_year :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> year
  (** Input JSON data of type {!year}. *)

val year_of_string :
  string -> year
  (** Deserialize JSON data of type {!year}. *)

val write_maybe :
  Bi_outbuf.t -> maybe -> unit
  (** Output a JSON value of type {!maybe}. *)

val string_of_maybe :
  ?len:int -> maybe -> string
  (** Serialize a value of type {!maybe}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_maybe :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> maybe
  (** Input JSON data of type {!maybe}. *)

val maybe_of_string :
  string -> maybe
  (** Deserialize JSON data of type {!maybe}. *)

val write_search_entry :
  Bi_outbuf.t -> search_entry -> unit
  (** Output a JSON value of type {!search_entry}. *)

val string_of_search_entry :
  ?len:int -> search_entry -> string
  (** Serialize a value of type {!search_entry}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_search_entry :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> search_entry
  (** Input JSON data of type {!search_entry}. *)

val search_entry_of_string :
  string -> search_entry
  (** Deserialize JSON data of type {!search_entry}. *)

val write_search_result :
  Bi_outbuf.t -> search_result -> unit
  (** Output a JSON value of type {!search_result}. *)

val string_of_search_result :
  ?len:int -> search_result -> string
  (** Serialize a value of type {!search_result}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_search_result :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> search_result
  (** Input JSON data of type {!search_result}. *)

val search_result_of_string :
  string -> search_result
  (** Deserialize JSON data of type {!search_result}. *)

val write_query_entry :
  Bi_outbuf.t -> query_entry -> unit
  (** Output a JSON value of type {!query_entry}. *)

val string_of_query_entry :
  ?len:int -> query_entry -> string
  (** Serialize a value of type {!query_entry}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_query_entry :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> query_entry
  (** Input JSON data of type {!query_entry}. *)

val query_entry_of_string :
  string -> query_entry
  (** Deserialize JSON data of type {!query_entry}. *)

