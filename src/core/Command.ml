
(** {1 Command Type} *)

open Lwt.Infix

type res =
  | Cmd_match of unit Lwt.t
  | Cmd_skip
  | Cmd_fail of string

type t = {
  prio: int; (* priority. The lower, the more urgent this command is. *)
  match_: Core.t -> Core.privmsg -> res;
  name: string;
  descr: string; (* for !help *)
}

let make ?(descr="") ?(prio=99) ~name f =
  { descr; prio; name; match_=f; }

let extract_hl s =
  try
    let i = String.rindex s '>' in
    if i < String.length s-1 then (
      let hl =
        String.sub s (i+1) (String.length s-i-1) |> String.trim
      in
      let s = String.sub s 0 i |> String.trim in
      Some (s, hl)
    ) else None
  with Not_found -> None

let match_prefix1_full ~prefix msg : (string * string option) option =
  let re = Str.regexp (Printf.sprintf "^![ ]*%s\\b[ ]*\\(.*\\)$" prefix) in
  begin match Prelude.re_match1 Prelude.id re msg.Core.message with
    | None -> None
    | Some matched ->
      let matched = String.trim matched in
      match extract_hl matched with
        | None -> Some (matched, None)
        | Some (a,b) -> Some (a, Some b)
  end

let match_prefix1 ~prefix msg =
  Prelude.map_opt fst (match_prefix1_full ~prefix msg)

exception Fail of string

let make_simple_inner_ ~query ?descr ?prio ~prefix f : t =
  let match_ (module C:Core.S) msg =
    match match_prefix1_full ~prefix msg with
      | None -> Cmd_skip
      | Some (sub, hl) ->
        (* Log.logf "command `%s` matched with %s, hl=%s"
          prefix sub (match hl with None -> "none" | Some h -> h); *)
        try
          let fut =
            f msg sub >>= fun lines ->
            let lines = match hl with
              | None -> lines
              | Some hl -> List.map (fun line -> hl ^ ": " ^ line) lines
            in
            let target = if query then Core.nick msg else Core.reply_to msg in
            let delay = if query then Some 0.5 else None in
            C.send_privmsg_l_nolimit ?delay ~target ~messages:lines ()
          in
          Cmd_match fut
        with Fail msg ->
          Cmd_fail msg
  in
  make ?descr ?prio ~name:prefix match_

let make_simple_l ?descr ?prio ~prefix f : t =
  make_simple_inner_ ~query:false ?descr ?prio ~prefix f

let make_simple_query_l ?descr ?prio ~prefix f : t =
  make_simple_inner_ ~query:true ?descr ?prio ~prefix f

let make_simple ?descr ?prio ~prefix f : t =
  make_simple_l ?descr ?prio ~prefix
    (fun msg s -> f msg s >|= function
       | None -> []
       | Some x -> [x])

let compare_prio c1 c2 = compare c1.prio c2.prio

(* help command *)
let cmd_help (l:t list): t =
  make_simple ~descr:"help message" ~prefix:"help" ~prio:5
    (fun _ s ->
       let s = String.trim s in
       let res =
         if s=""
         then
           let l = List.map (fun c -> c.name) l in
           let message =
             "!help: commands are " ^ Prelude.string_list_to_string l
           in
           Some message
         else
           try
             let c = List.find (fun c -> c.name = s) l in
             Some (Printf.sprintf "%s: %s (prio %d)" c.name c.descr c.prio)
           with Not_found ->
             Some ("error: unknown command " ^ s)
       in
       Lwt.return res
    )

let run core l msg : unit Lwt.t =
  let rec aux = function
    | [] ->
      Log.logf "no command found for %s" (Core.string_of_privmsg msg);
      Lwt.return_unit
    | c :: tail ->
      begin match c.match_ core msg with
        | Cmd_skip -> aux tail
        | Cmd_match f ->
          Log.logf "command %s succeeded for %s"
            c.name (Core.string_of_privmsg msg);
          f
        | Cmd_fail e ->
          Log.logf "command %s failed on %s with %s"
            c.name (Core.string_of_privmsg msg) e;
          aux tail
      end
  in
  aux (cmd_help l :: l)
