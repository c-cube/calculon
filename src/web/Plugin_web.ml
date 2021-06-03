
(** {1 Commands querying the Web} *)

open Calculon

open Soup
open Lwt.Infix
module Log = Core.Log

let get_body uri =
  Lwt_preemptive.detach
    (fun () ->
       Curly.run ~args:["-L"]
         Curly.(Request.make ~url:(Uri.to_string uri) ~meth:`GET ()))
    ()
  >>= function
  | Ok {Curly.Response. body;_ } -> Lwt.return body
  | Error e -> Lwt.fail (Failure (Format.asprintf "%a" Curly.Error.pp e))

let page_title ~with_description uri =
  get_body uri >>= fun body ->
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
    Some msg |> Lwt.return
  | Some title, None ->
    let msg = Format.asprintf "%s" title in
    (* Log.logf "og:title %s" msg; *)
    Some msg |> Lwt.return
  | _, _ ->
    parse body $ "title" |> leaf_text |> Lwt.return


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
       | _ -> Lwt.return_none
    )

let find_yt_ids ?(n=1) (body:string): string list =
  let ast = parse body in
  Soup.select "#results li li > div" ast
  |> Soup.to_list
  |> CCList.take n
  |> CCList.filter_map (Soup.attribute "data-context-item-id")
  |> List.map (fun id -> "https://youtube.com/watch?v=" ^ id)

let get_youtube_search (query:string): string Lwt.t =
  let uri =
    Uri.of_string "https://www.youtube.com/results"
  in
  let uri = Uri.add_query_params' uri ["sp","EgIQAQ%3D%3D"; "q", query] in
  Lwt.catch
    (fun () -> get_body uri)
    (function
      | Failure e ->
        Log.err (fun k->k "error in fetching `%s`:\n%s" query e);
        Lwt.return ""
      | e ->
        Log.err (fun k->k "error in fetching `%s`:\n%s" query @@ Printexc.to_string e);
        Lwt.return "")


let cmd_yt_search =
  Command.make_simple_l
    ~prio:10 ~cmd:"yt_search" ~descr:"lookup on youtube"
    (fun _ s ->
       Log.debug (fun k->k"yt_search `%s`" s);
       begin
         get_youtube_search (String.trim s) >|= fun body ->
         Log.debug (fun k->k"yt_search: body of size %d" (String.length body));
         find_yt_ids ~n:1 body
       end
       >>= fun urls ->
       Lwt_list.fold_left_s (fun acc url ->
           Log.debug (fun k->k "Getting metadata for url: `%s`" url);
           page_title ~with_description:false (Uri.of_string url) >>= function
           | Some x ->
             let descr = Format.asprintf "%s : %s" url x in
             descr::acc |> Lwt.return
           | None ->
             url::acc |> Lwt.return
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

  let search s: string option Lwt.t =
    let uri = mk_query s in
    Lwt.catch
      (fun () ->
        get_body uri >|= fun s ->
        try
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
      )
      (fun _ -> Lwt.return None)

  let cmd =
    Command.make_simple
      ~prio:10 ~cmd:"giphy" ~descr:"lookup on giphy (Powered by Giphy)"
      (fun _ s ->
         let s = String.trim s in
         if s=""
         then Lwt.return_none
         else search (String.trim s) >>= function
           | Some x -> Lwt.return (Some x)
           | None -> Lwt.return (Some "not found")
      )
end

let cmd_emoji =
  let find_h1 body =
    let ast = Soup.parse body in
    Soup.select "article h1" ast
    |> Soup.to_list
    |> CCList.take 1
    |> CCList.map
      (fun n ->
         Soup.select "#emoji" n |> Soup.to_list
         |> List.map Soup.to_string |> String.concat " ",
         Soup.children n |> Soup.to_list |> CCList.take 1
         |> List.map Soup.to_string |> String.concat " ")
    |> CCList.head_opt
  and find_search body =
    let ast = Soup.parse body in
    ast
    |> Soup.select "article #search-results li a"
    |> Soup.to_list |> CCList.take 1
    |> CCList.find_map (Soup.attribute "href")
  in

  Command.make_simple ~descr:"look for emojis" ~cmd:"emoji" ~prio:10
    (fun _msg s ->
       let s = String.trim s in
       let query = Printf.sprintf "https://emojipedia.org/search/?q=%s" s in
       Log.debug (fun k->k "emoji: query is '%s'" query);
       Lwt.catch
         (fun () ->
            get_body (Uri.of_string query) >|= fun s ->
            match find_h1 s, find_search s with
            | Some (em,title), _ ->
              Some (Printf.sprintf "%s: %s (%s)" em title query)
            | None, Some href ->
              Some href
            | None, None -> Some (Printf.sprintf "not found"))
         (fun e ->
            Log.err (fun k->k "emoji: query failed:@.%s" (Printexc.to_string e));
            Lwt.return None))

let plugin =
  [ cmd_yt;
    cmd_yt_search;
    Giphy.cmd;
    cmd_emoji;
  ] |> Plugin.of_cmds
