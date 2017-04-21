
(** {1 Commands querying the Web} *)

open Calculon

open Cohttp_lwt_unix
open Soup
open Lwt.Infix

let page_title ~with_description uri =
  let open Cohttp in
  let rec get_body uri =
    Client.get uri >>= fun (resp, body) ->
    if Code.(is_redirection (code_of_status resp.Response.status)) then
      Header.get resp.Response.headers "location"
      |> CCOpt.get_exn
      |> Uri.of_string
      |> get_body
    else
      Cohttp_lwt_body.to_string body
  in
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
    ~prio:10 ~prefix:"yt" ~descr:"lookup description of given youtube URL"
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
  Cohttp_lwt_unix.Client.get uri >>= fun (_,body) ->
  Cohttp_lwt_body.to_string body

let cmd_yt_search =
  Command.make_simple_l
    ~prio:10 ~prefix:"yt_search" ~descr:"lookup on youtube"
    (fun _ s ->
       (get_youtube_search (String.trim s) >|= fun body ->
        find_yt_ids ~n:1 body)
       >>= fun urls ->
       Lwt_list.fold_left_s (fun acc url ->
           Log.logf "Getting metadata for url %s" url;
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

  let mk_query s: Uri.t =
    Uri.add_query_params
      (Uri.of_string"http://api.giphy.com/v1/gifs/search")
      [ "q", [s];
        "api_key", [api_key];
      ]

  let search s: string option Lwt.t =
    let uri = mk_query s in
    Lwt.catch
      (fun () ->
        Cohttp_lwt_unix.Client.get uri >>= fun (_,body) ->
        Cohttp_lwt_body.to_string body >|= fun s ->
        try
          let r = Giphy_j.search_result_of_string s in
          begin match r.Giphy_j.data with
            | r :: _ -> Some r.Giphy_j.url
            | [] -> None
          end
        with _ -> None
      )
      (fun _ -> Lwt.return None)

  let cmd =
    Command.make_simple
      ~prio:10 ~prefix:"giphy" ~descr:"lookup on giphy (Powered by Giphy)"
      (fun _ s ->
         let s = String.trim s in
         if s=""
         then Lwt.return_none
         else search (String.trim s) >>= function
           | Some x -> Lwt.return (Some x)
           | None -> Lwt.return (Some "not found")
      )
end

let plugin =
  [ cmd_yt;
    cmd_yt_search;
    Giphy.cmd;
  ] |> Plugin.of_cmds
