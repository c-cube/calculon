(** These data-structures are for holding meta-data
    expressed by the {{:http://ogp.me/}open graph protocol}.

    The basic information represented is the site name, title, content type,
    canonical url and a preview image. If the content is music or video data,
    additional data, additional data like tags or length may be present. Even
    though the protocal requires a minimum information of title, type, image
    and url, the webpage might be malformed. Parse.parse_string therefore just
    returns a list of og_metadata.
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
                        og_url   : url
                      }

val parse_url : string -> url option
val parse_locale : string -> locale option
val parse_type  : string -> og_type

val make_title : string -> og_metadata
val make_type  : og_type -> og_metadata
val make_image : url -> og_metadata
val make_audio : url -> og_metadata
val make_description : string -> og_metadata
val make_determiner : og_determiner -> og_metadata
val make_locale : locale -> og_metadata
val make_alternate_locale : locale -> og_metadata
val make_site_name : string -> og_metadata
val make_video : url -> og_metadata
val make_video_metadata : og_video_metadata -> og_metadata
val make_url : url -> og_metadata

val make_video_tag      : string -> og_video_metadata
val make_video_duration : int -> og_video_metadata

val format_metadata : Format.formatter -> og_metadata -> unit

module Parser : sig
  val parse_string : string -> og_metadata list
end
