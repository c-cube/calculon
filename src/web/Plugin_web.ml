
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
  let uri = Uri.add_query_param' uri ("search_query", query) in
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

let plugin =
  [ cmd_yt;
    cmd_yt_search;
  ] |> Plugin.of_cmds
