(* Auto-generated from "giphy.atd" *)


type search_entry = { type_: string; url: string; embed_url: string }

type json = Yojson.Safe.json

type search_result = {
  data: search_entry list;
  meta: json;
  pagination: json
}
