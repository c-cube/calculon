(* Auto-generated from "giphy.atd" *)


type search_entry = Giphy_t.search_entry = {
  type_: string;
  url: string;
  embed_url: string
}

type json = Yojson.Safe.json

type search_result = Giphy_t.search_result = {
  data: search_entry list;
  meta: json;
  pagination: json
}

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

val write_json :
  Bi_outbuf.t -> json -> unit
  (** Output a JSON value of type {!json}. *)

val string_of_json :
  ?len:int -> json -> string
  (** Serialize a value of type {!json}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_json :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> json
  (** Input JSON data of type {!json}. *)

val json_of_string :
  string -> json
  (** Deserialize JSON data of type {!json}. *)

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

