(** These data-structures are for holding meta-data
    expressed by the {{:http://ogp.me/}graph protocol}.
*)

type url = { name : string }

type locale = { language : string; territory : string }

type og_type = TMusic | TVideo | TWebsite |
               TArticle | TBook | TProfile | TOther of string

type og_determiner = A | An | The | NoDet | Auto

type og_video_metadata =
  | VTag of string
  | VDuration of int

type og_metadata =
  | Title of string | Type of og_type | Image of url | Url of url
  | Audio of url | Description of string | Determiner of og_determiner
  | Locale of locale | AlternateLocale of locale | SiteName of string
  | Video of url | VideoMeta of og_video_metadata | UnparsedMeta of string

type basic_metadata = { og_title : string;
                        og_type  : og_type;
                        og_image : url;
                        og_url   : url;
                      }

let parse_type = function
  | "video" -> TVideo
  | "music" -> TMusic
  (* TODO: add more types / subtypes *)
  | str -> TOther str

let parse_url str = Some { name = str }

let parse_locale str =
  match Re.split (Re.str "_" |> Re.compile) str with
  | language :: territory :: _ -> Some { language; territory }
  | _ -> None

let make_title str = Title str
let make_type t = Type t
let make_image url = Image url
let make_audio url = Audio url
let make_description str =  Description str
let make_determiner det = Determiner det
let make_locale l = Locale l
let make_alternate_locale l = AlternateLocale l
let make_site_name str = SiteName str
let make_video url = Video url
let make_video_metadata meta = VideoMeta meta
let make_url url = Url url

let make_video_tag tag = VTag tag
let make_video_duration dur = VDuration dur

let format_url formatter { name } =
  Format.fprintf formatter "%s" name

let format_locale formatter { language; territory } =
  Format.fprintf formatter "%s_%s" language territory

let format_determiner formatter = function
  | A     -> Format.fprintf formatter "a"
  | An    -> Format.fprintf formatter "an"
  | The   -> Format.fprintf formatter "the"
  | NoDet -> Format.fprintf formatter ""
  | Auto  -> Format.fprintf formatter ""

let format_type formatter t =
  let str = match t with
    | TMusic -> "music"
    | TVideo -> "video"
    | TWebsite -> "website"
    | TArticle -> "article"
    | TBook -> "book"
    | TProfile -> "profile"
    | TOther str -> "other:"^str
  in Format.fprintf formatter "%s" str


let format_video_metadata formatter = function
  | VTag tag -> Format.fprintf formatter "tag: %s" tag
  | VDuration dur -> Format.fprintf formatter "duration: %d s" dur

let format_metadata formatter = function
  | Title t ->
    Format.fprintf formatter "Title: %s" t
  | Type t ->
    Format.fprintf formatter "Type: %a" format_type t
  | Image url ->
    Format.fprintf formatter "Image: %a" format_url url
  | Url url ->
    Format.fprintf formatter "Url: %a" format_url url
  | Audio url ->
    Format.fprintf formatter "Audio: %a" format_url url
  | Description str  ->
    Format.fprintf formatter "Description: %s" str
  | Determiner og_determiner ->
    Format.fprintf formatter "Determiner %a" format_determiner og_determiner
  | Locale locale ->
    Format.fprintf formatter "Locale: %a" format_locale locale
  | AlternateLocale locale  ->
    Format.fprintf formatter "Alternate Locale: %a" format_locale locale
  | SiteName str  ->
    Format.fprintf formatter "Site Name: %s" str
  | Video url  ->
    Format.fprintf formatter "Video: %a" format_url url
  | VideoMeta meta ->
    Format.fprintf formatter "Video %a" format_video_metadata meta
  | UnparsedMeta name ->
    Format.fprintf formatter "Unparsed tag: %s" name

module Parser = struct
  open Soup
  let og_prefix = Re.Perl.compile_pat "^og:"

  let og_parser list elem =
    let prop constructor x list =
      match attribute "content" x with
      | None -> list
      | Some str -> constructor str :: list
    in
    let optprop constructor x list =
      match attribute "content" x with
      | None -> list
      | Some str -> match constructor str with
        | Some elem -> elem :: list
        | None -> list
    in
    let optparser parser constructor str =
      match parser str with
      | None -> None
      | Some url -> Some (constructor url)
    in
    let purl = optparser parse_url in
    let plocale = optparser parse_locale in
    match attribute "property" elem with
    | Some "og:title" -> prop make_title elem list
    | Some "og:type" -> prop (fun x -> make_type (parse_type x)) elem list
    | Some "og:image" -> optprop (purl make_image) elem list
    | Some "og:url" -> optprop (purl make_url) elem list
    | Some "og:audio" -> optprop (purl make_audio) elem list
    | Some "og:description" -> prop make_description elem list
    | Some "og:determiner" -> (Determiner Auto) :: list
    | Some "og:locale" -> optprop (plocale make_locale) elem list
    | Some "og:locale:alternate" -> optprop (plocale make_locale) elem list
    | Some "og:site_name" -> prop make_site_name elem list
    | Some "og:video" -> optprop (purl make_video) elem list
    | Some "og:video:tag" ->
      prop (fun x -> x |> make_video_tag |> make_video_metadata ) elem list
    | Some str when Re.execp og_prefix str ->
      UnparsedMeta str :: list
    | Some _ -> list
    | None -> list

  let parse_string content =
    let soup = parse content in
    let meta_tags = soup $$ "meta" in
    let og_nodes = fold og_parser [] meta_tags in
    List.rev og_nodes
end
