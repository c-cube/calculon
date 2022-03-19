
(** {1 Commands querying the Web} *)

open Calculon

open Soup
module Log = Core.Log

let get_body uri =
  Curly.run ~args:["-L"]
    Curly.(Request.make ~url:(Uri.to_string uri) ~meth:`GET ())
  |> function
  | Ok {Curly.Response. body;_ } -> body
  | Error e -> raise (Failure (Format.asprintf "%a" Curly.Error.pp e))

let page_title ~with_description uri =
  get_body uri |> fun body ->
  let tags = Og.Parser.parse_string body in
  let title_descr = List.fold_left (fun (t,d) -> function
      | Og.Title title       when t = None && title <> "" -> (Some title, d)
      | Og.Description descr when d = None && descr <> "" && with_description ->
        (t, Some descr)
      | _ ->
        (t,d)
    ) (None, None) tags in
  match title_descr with
  | Some title, Some description ->
    let msg = Format.asprintf "%s : %s" title description in
    (*    Log.logf "og:: %s" msg; *)
    Some msg
  | Some title, None ->
    let msg = Format.asprintf "%s" title in
    (* Log.logf "og:title %s" msg; *)
    Some msg
  | _, _ ->
    parse body $ "title" |> leaf_text


let youtube_hosts = [
  "youtube.com"; "www.youtube.com";
  "youtu.be"; "www.youtu.be";
]

let cmd_yt =
  Command.make_simple
    ~prio:10 ~cmd:"yt" ~descr:"lookup description of given youtube URL"
    (fun _ s ->
       let uri = Uri.of_string (String.trim s) in
       match Uri.host uri with
       | Some host when List.mem host youtube_hosts ->
         page_title ~with_description:true uri
       | _ -> None
    )

let find_yt_ids ?(n=1) (body:string): string list =
  let ast = parse body in
  Soup.select "#results li li > div" ast
  |> Soup.to_list
  |> CCList.take n
  |> CCList.filter_map (Soup.attribute "data-context-item-id")
  |> List.map (fun id -> "https://youtube.com/watch?v=" ^ id)

let get_youtube_search (query:string): string =
  let uri =
    Uri.of_string "https://www.youtube.com/results"
  in
  let uri = Uri.add_query_params' uri ["sp","EgIQAQ%3D%3D"; "q", query] in
  try get_body uri
  with
  | Failure e ->
    Log.err (fun k->k "error in fetching `%s`:\n%s" query e);
    ""
  | e ->
    Log.err (fun k->k "error in fetching `%s`:\n%s" query @@ Printexc.to_string e);
    ""


let cmd_yt_search =
  Command.make_simple_l
    ~prio:10 ~cmd:"yt_search" ~descr:"lookup on youtube"
    (fun _ s ->
       Log.debug (fun k->k"yt_search `%s`" s);
       let urls =
         get_youtube_search (String.trim s) |> fun body ->
         Log.debug (fun k->k"yt_search: body of size %d" (String.length body));
         find_yt_ids ~n:1 body
       in
       List.fold_left (fun acc url ->
           Log.debug (fun k->k "Getting metadata for url: `%s`" url);
           page_title ~with_description:false (Uri.of_string url) |> function
           | Some x ->
             let descr = Format.asprintf "%s : %s" url x in
             descr::acc
           | None ->
             url::acc
         ) [] urls
    )

(* see https://github.com/Giphy/GiphyAPI *)
module Giphy = struct
  let api_key = "dc6zaTOxFJmzC"

  let limit = 5

  let mk_query s: Uri.t =
    Uri.add_query_params
      (Uri.of_string"http://api.giphy.com/v1/gifs/search")
      [ "q", [s];
        "api_key", [api_key];
        "limit", [string_of_int limit]
      ]

  let search s: string option =
    let uri = mk_query s in
    try
      let s = get_body uri in
      Log.debug (fun k->k"query to giphy returned:@.%s" s);
      let r = Giphy_j.search_result_of_string s in
      begin match r.Giphy_j.data with
        | [] ->
          Log.debug (fun k->k"giphy: no data");
          None
        | l ->
          let r = Prelude.random_l l in
          Log.info
            (fun k->k "giphy: pick `%s` in list of len %d"
                r.Giphy_j.url (List.length l));
          let images = r.Giphy_j.images in
          begin match images.Giphy_j.images_original, images.Giphy_j.images_downsized with
            | Some i, _ -> Some i.Giphy_j.i_url
            | None, Some i -> Some i.Giphy_j.i_url
            | None, None ->
              (* default: return the embed_url *)
              Log.err (fun k->k
                          "giphy: could not get `original` or `downsized` picture for `%s`"
                          r.Giphy_j.url);
              Some r.Giphy_j.embed_url
          end
      end
    with _ -> None

  let cmd =
    Command.make_simple
      ~prio:10 ~cmd:"giphy" ~descr:"lookup on giphy (Powered by Giphy)"
      (fun _ s ->
         let s = String.trim s in
         if s=""
         then None
         else search (String.trim s) |> function
           | Some x -> Some x
           | None -> Some "not found"
      )
end

let find_emoji (s:string) : string option =
  let find_h1 ast =
    let open CCOpt.Infix in
    let open Soup in
    let* h1 = ast $? "article h1" in
    let+ em = h1 $? ".emoji" >>= Soup.leaf_text
    and+ descr = h1 |> children |> last >>= leaf_text in
    em, descr
  and find_search ast  =
    let open CCOpt.Infix in
    let* a = ast $? ".search-results > li a" in
    let+ href = Soup.attribute "href" a in
    "https://emojipedia.org/"^ href
  in

  let query = Printf.sprintf "https://emojipedia.org/search/?q=%s" s in
  Log.debug (fun k->k "emoji: query is '%s'" query);
  try
    get_body (Uri.of_string query) |> fun body ->
    let ast = Soup.parse body in
    match find_h1 ast, find_search ast with
    | Some (em,title), _ ->
      Some (Printf.sprintf "%s: %s (%s)" em title query)
    | None, Some href ->
      Some href
    | None, None -> Some (Printf.sprintf "not found")
  with e ->
    Log.err (fun k->k "emoji: query failed:@.%s" (Printexc.to_string e));
    None

let cmd_emoji =
  Command.make_simple ~descr:"look for emojis" ~cmd:"emoji" ~prio:10
    (fun _msg s ->
       let s = String.trim s in
       find_emoji s)

let plugin =
  [ cmd_yt;
    cmd_yt_search;
    Giphy.cmd;
    cmd_emoji;
  ] |> Plugin.of_cmds
