(* Auto-generated from "giphy.atd" *)


type image = { i_url: string }

type images = {
  images_original: image option;
  images_downsized: image option
}

type search_entry = {
  type_: string;
  url: string;
  embed_url: string;
  images: images
}

type json = Yojson.Safe.json

type search_result = {
  data: search_entry list;
  meta: json;
  pagination: json
}
