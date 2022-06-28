open Calculon
open CCFun
open Lwt_infix

let get_body uri =
  let run () =
    Curly.run ~args:[ "-L" ]
      Curly.(Request.make ~url:(Uri.to_string uri) ~meth:`GET ())
    |> function
    | Ok { Curly.Response.body; _ } -> body
    | Error e -> raise (Failure (Format.asprintf "%a" Curly.Error.pp e))
  in
  Lwt_preemptive.detach run ()

type query = Movie of string | Serie of string

let query_movie s = Movie s
let query_serie s = Serie s
let api_prefix = Uri.of_string "http://omdbapi.com/"

let make_search_uri query =
  let title, kind =
    match query with
    | Movie title -> title, "movie"
    | Serie title -> title, "series"
  in
  Uri.add_query_params' api_prefix
    [ "v", "1"; "r", "json"; "s", title; "type", kind ]

let make_get_uri id =
  Uri.add_query_params' api_prefix [ "v", "1"; "r", "json"; "i", id ]

let parse_search body =
  try Movie_j.search_result_of_string body
  with exn ->
    Printf.printf "invalid imdb search response (%s) : %S"
      (Printexc.to_string exn) body;
    { Movie_t.results = []; count = 0 }

let parse_get body =
  try Option.some @@ Movie_j.query_entry_of_string body
  with exn ->
    Printf.printf "invalid imdb query response (%s) : %S"
      (Printexc.to_string exn) body;
    None

let search query = make_search_uri query |> get_body >|= parse_search
let get_infos id = make_get_uri id |> get_body >|= parse_get

let ellipsis n s =
  if String.length s > n then (
    try
      let last = String.rindex_from s n ' ' in
      let s = Bytes.sub (Bytes.of_string s) 0 (last + 4) in
      Bytes.blit_string "... " 0 s last 4;
      Bytes.to_string s
    with Not_found -> CCString.take n s
  ) else
    s

let make_imdb_url id = String.concat "/" [ "http://www.imdb.com/title"; id ]

let show_result ?buffer (title, r) =
  let open Printf in
  let open Movie_t in
  let buffer = buffer |> Option.get_or_lazy (fun () -> Buffer.create 10) in
  Buffer.clear buffer;
  bprintf buffer "%S " (ellipsis 50 title);
  Option.iter (bprintf buffer "(%d) ") r.year;
  bprintf buffer "- %.1f " r.rating;
  Option.iter (Buffer.add_string buffer % ellipsis 150) r.plot;
  Buffer.add_string buffer @@ make_imdb_url r.id;
  Buffer.contents buffer

let title { Movie_t.s_title; _ } = s_title
let get_id { Movie_t.s_id; _ } = s_id

let refine_results ?(n = 3) results =
  results.Movie_t.results
  |> CCList.filter (Option.is_some % title)
  |> CCList.take n |> List.map get_id
  |> Lwt_list.filter_map_p get_infos

let format_seq ?n results =
  let buffer = Buffer.create 100 in
  let title { Movie_t.title; _ } = title in
  let* results = refine_results ?n results in
  let* l =
    Lwt_list.filter_map_p
      (fun r -> Lwt.return @@ Option.map (flip CCPair.make r) @@ title r)
      results
  in
  Lwt_list.map_p (fun x -> Lwt.return @@ show_result ~buffer x) l

let mk_movie_cmd cmd q_of_str =
  Command.make_simple_l ~descr:"look for movies/series" ~prio:10 ~cmd
    (fun _ s -> String.trim s |> q_of_str |> search >>= format_seq)

let cmd_film = mk_movie_cmd "film" query_movie
let cmd_serie = mk_movie_cmd "serie" query_serie
let plugin = [ cmd_film; cmd_serie ] |> Plugin.of_cmds
