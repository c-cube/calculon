
open Calculon

open Lwt.Infix
open Cohttp_lwt_unix
open CCFun

type query =
  | Movie of string
  | Serie of string

let query_movie s = Movie s
let query_serie s = Serie s

let api_prefix = Uri.of_string "http://omdbapi.com/"

let make_search_uri query =
  let title, kind =
    match query with
    | Movie title -> title, "movie"
    | Serie title -> title, "series"
  in
  Uri.add_query_params' api_prefix ["v", "1"; "r", "json"; "s", title; "type", kind ]

let make_get_uri id =
  Uri.add_query_params' api_prefix ["v", "1"; "r", "json"; "i", id ]

let parse_search body =
  try
    Movie_j.search_result_of_string body 
  with exn ->
    Printf.printf "invalid imdb search response (%s) : %S" (Printexc.to_string exn) body;
    { Movie_t.results = []; count = 0 }

let parse_get body =
  try
    CCOpt.pure @@ Movie_j.query_entry_of_string body 
  with exn ->
    Printf.printf "invalid imdb query response (%s) : %S" (Printexc.to_string exn) body;
    None

let search query =
  make_search_uri query |>
  Client.get >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >|=
  parse_search

let get_infos id =
  make_get_uri id |>
  Client.get >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >|=
  parse_get

let ellipsis n s =
  if String.length s > n then begin
    try
      let last = String.rindex_from s n ' ' in
      let s = Bytes.sub (Bytes.of_string s) 0 (last + 4) in
      Bytes.blit_string "... " 0 s last 4;
      Bytes.to_string s
    with Not_found ->
      CCString.take n s
  end else
    s

let make_imdb_url id =
  String.concat "/" ["http://www.imdb.com/title"; id ]

let show_result ?buffer (title, r) =
  let open Printf in
  let open CCOpt in
  let open Movie_t in
  let buffer = get_lazy (fun () -> Buffer.create 10) buffer in
  Buffer.clear buffer;
  bprintf buffer "%S " (ellipsis 50 title);
  iter (bprintf buffer "(%d) ") r.year;
  bprintf buffer "- %.1f " r.rating;
  iter (Buffer.add_string buffer % ellipsis 150) r.plot;
  Buffer.add_string buffer @@ make_imdb_url r.id;
  Buffer.contents buffer

let title { Movie_t.s_title; _ } = s_title
let get_id { Movie_t.s_id; _ }  = s_id 

let refine_results ?(n=3) results =
  results.Movie_t.results |>
  CCList.filter (CCOpt.is_some % title) |>
  CCList.take n |>
  CCList.map get_id |>
  Lwt_list.filter_map_p get_infos

let format_seq ?n results =
  let buffer = Buffer.create 100 in
  let title { Movie_t.title; _ } = title in
  refine_results ?n results >>= fun results ->
  results |>
  CCList.filter_map (fun r -> CCOpt.map (flip CCPair.make r) @@ title r) |>
  CCList.map (show_result ~buffer) |>
  Lwt.return

let mk_movie_cmd prefix cmd q_of_str =
  Command.make_simple_l
    ~descr:"look for movies/series" ~prio:10 ?prefix ~cmd
    (fun _ s ->
       String.trim s |>
       q_of_str |>
       search >>=
       format_seq
    )

let cmd_film = mk_movie_cmd (Some "!") "film" query_movie
let cmd_serie = mk_movie_cmd (Some "!") "serie" query_serie

let plugin =
  [ cmd_film;
    cmd_serie;
  ] |> Plugin.of_cmds
