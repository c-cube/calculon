
(** {1 Small Parser for IRC Logs} *)

type 'a sequence = ('a -> unit) -> unit

type log_record = {
  author: string;
  time: string;
  msg: string;
}

let string_of_record r =
  Printf.sprintf "{author=%s, time=%s, msg=%s}" r.author r.time r.msg

let pp_record out r =
  Format.fprintf out "{author=%s, time=%s, msg=%s}" r.author r.time r.msg

let re_irssi = Re_posix.re "([0-9:]*)<([^>]*)> (.*)" |> Re.compile

let re_weechat = Re_posix.re "([0-9 :]*)\t([^>]*)\t(.*)" |> Re.compile

type fmt =
  | Irssi
  | Weechat

let re_of_fmt = function
  | Irssi -> re_irssi
  | Weechat -> re_weechat

let fmt_of_string = function
  | "irssi" -> Irssi
  | "weechat" -> Weechat
  | s -> invalid_arg ("unknown Irclog.fmt: " ^ s)

let string_of_fmt = function
  | Irssi -> "irssi"
  | Weechat -> "weechat"

let fmt_l = List.map string_of_fmt [Irssi; Weechat]

(* read lines *)
let rec seq_lines_ ic yield =
  match input_line ic with
    | s -> yield s; seq_lines_ ic yield
    | exception End_of_file -> ()

let norm_author s =
  if s="" then s
  else match s.[0] with
    | '+' | '@' -> String.sub s 1 (String.length s-1)
    | _ -> s

let parse_record fmt s =
  let re = re_of_fmt fmt in
  begin match Re.exec_opt re s with
    | None -> None
    | Some g ->
      let time = Re.Group.get g 1 |> String.trim in
      let author = Re.Group.get g 2 |> String.trim |> norm_author in
      let msg = Re.Group.get g 3 in
      (* check if this line is useless *)
      begin match author, fmt with
        | ("--" | "<--" | "-->"), Weechat -> None (* join/part *)
        | _ -> Some {author; time; msg}
      end
  end

let seq_record_ fmt ic yield =
  seq_lines_ ic
    (fun l -> match parse_record fmt l with
       | None -> ()
       | Some r -> yield r)

let iter_file fmt file yield =
  CCIO.with_in file (fun ic -> seq_record_ fmt ic yield)

let rec seq_files_ dir yield =
  let d = Unix.opendir dir in
  CCFun.finally1
    ~h:(fun () -> Unix.closedir d)
    (fun d ->
       let rec aux () = match Unix.readdir d with
         | s ->
           let abs_s = Filename.concat dir s in
           begin
             if s = "." || s = ".."  then ()
             else if Sys.is_directory abs_s
             then seq_files_ abs_s yield
             else yield abs_s
           end;
           aux ()
         | exception End_of_file -> ()
       in
       aux ())
    d

let iter_dir fmt dir yield =
  seq_files_ dir
    (fun file ->
       CCIO.with_in file
         (fun ic -> seq_record_ fmt ic (fun x -> yield (file,x))))

let iter_file_or_dir fmt s =
  if Sys.is_directory s
  then
    seq_files_ s
    |> Sequence.flat_map (iter_file fmt)
  else iter_file fmt s

