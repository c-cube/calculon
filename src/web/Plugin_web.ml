
(** {1 Commands querying the Web} *)

open Cohttp_lwt_unix
open Soup
open Lwt.Infix

let page_title uri =
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
           page_title uri
         | _ -> Lwt.return_none
    )

let cancer_uri =
  Uri.of_string "https://polochon.lelele.io/cancer/quickcancer"

let cmd_cancer =
  Command.make_simple
    ~prio:10 ~prefix:"cancer" ~descr:"lookup in the abyss of bad videos"
    (fun _ s ->
       Client.get cancer_uri >>= fun (_, body) ->
       Cohttp_lwt_body.to_string body >>= fun body ->
       let fmt_link (title, url) = title ^ ":" ^ url in
       let links =
         Str.split (Str.regexp "\n") body
         |> List.filter ((<>) "")
         |> CCList.filter_map (fun s ->
           match Str.bounded_split (Str.regexp ":") s 2 with
             | [title; url] -> Some (title, url)
             | _ -> None)
       in
       let links_with_search =
         match String.trim s with
           | "" -> links
           | search ->
             let re = Str.regexp_case_fold search in
             CCList.filter_map
               (fun (title, url) ->
                  if Prelude.contains title re then Some (title, url) else None)
               links
       in

       if links_with_search = [] then Lwt.return_none
       else
         let message = fmt_link @@ DistribM.(run @@ uniform links_with_search) in
         Lwt.return (Some message)
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
       get_youtube_search (String.trim s) >|= fun body ->
       find_yt_ids body
    )

let plugin =
  [ cmd_yt;
    cmd_cancer;
    cmd_yt_search;
  ] |> Plugin.of_cmds
