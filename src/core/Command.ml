(** {1 Command Type} *)

open Lwt_infix

type res = Cmd_match of unit Lwt.t | Cmd_skip | Cmd_fail of string

type t = {
  prio: int;
  match_: prefix:string -> Core.t -> Core.privmsg -> res;
      (** How to react to incoming messages *)
  name: string;
  descr: string;
}

let make ?(descr = "") ?(prio = 99) ~name f = { descr; prio; name; match_ = f }

let extract_hl s =
  try
    let i = String.rindex s '>' in
    if i < String.length s - 1 then (
      let hl = String.sub s (i + 1) (String.length s - i - 1) |> String.trim in
      let s = String.sub s 0 i |> String.trim in
      Some (s, hl)
    ) else
      None
  with Not_found -> None

let match_prefix1_full ~prefix ~cmd msg : (string * string option) option =
  let re =
    Re.Perl.compile_pat (Printf.sprintf "^%s\\b[ ]*%s\\b[ ]*(.*)$" prefix cmd)
  in
  match Prelude.re_match1 id re msg.Core.message with
  | None -> None
  | Some matched ->
    let matched = String.trim matched in
    (match extract_hl matched with
    | None -> Some (matched, None)
    | Some (a, b) -> Some (a, Some b))

let match_prefix1 ~prefix ~cmd msg =
  Option.map fst (match_prefix1_full ~prefix ~cmd msg)

exception Fail of string

let make_simple_inner_ ~query ?descr ?prio ~cmd f : t =
  let match_ ~prefix (module C : Core.S) msg =
    match match_prefix1_full ~prefix ~cmd msg with
    | None -> Cmd_skip
    | Some (sub, hl) ->
      Core.Log.debug (fun k ->
          k "command '%s' matched with sub=%S, hl=%s" prefix sub
            (match hl with
            | None -> "none"
            | Some h -> Printf.sprintf "%S" h));
      (try
         let fut =
           let* lines = f msg sub in
           let lines =
             match hl with
             | None -> lines
             | Some hl -> List.map (fun line -> hl ^ ": " ^ line) lines
           in
           let target =
             if query then
               Core.nick msg
             else
               Core.reply_to msg
           in
           let delay =
             if query then
               Some 0.5
             else
               None
           in
           C.send_privmsg_l_nolimit ?delay ~target ~messages:lines ()
         in
         Cmd_match fut
       with Fail msg -> Cmd_fail msg)
  in
  make ?descr ?prio ~name:cmd match_

let make_simple_l ?descr ?prio ~cmd f : t =
  let descr =
    match descr with
    | None -> cmd
    | Some s -> s
  in
  make_simple_inner_ ~query:false ~descr ?prio ~cmd f

let make_simple_query_l ?descr ?prio ~cmd f : t =
  let descr =
    match descr with
    | Some s -> s
    | None -> cmd
  in
  make_simple_inner_ ~query:true ~descr ?prio ~cmd f

let make_custom ?descr ?prio ~name f =
  let match_ ~prefix:_ (module C : Core.S) msg =
    match f msg msg.Core.message with
    | None -> Cmd_skip
    | Some fut ->
      Cmd_match
        ( fut >>= fun lines ->
          let target = Core.reply_to msg in
          C.send_privmsg_l_nolimit ~target ~messages:lines () )
    | exception e -> Cmd_fail (Printexc.to_string e)
  in
  make ?descr ?prio ~name match_

let make_simple ?descr ?prio ~cmd f : t =
  make_simple_l ?descr ?prio ~cmd (fun msg s ->
      f msg s >|= function
      | None -> []
      | Some x -> [ x ])

let compare_prio c1 c2 = compare c1.prio c2.prio

(** Help command *)
let cmd_help (l : t list) : t =
  make_simple ~descr:"help message" ~cmd:"help" ~prio:5 (fun _ s ->
      let s = String.trim s in
      let res =
        match s with
        | "" ->
          let l = "help" :: List.map (fun c -> c.name) l in
          let message =
            "help: commands are " ^ Prelude.string_list_to_string l
          in
          Some message
        | "help" -> Some "displays help for commands"
        | _ ->
          (try
             let c = List.find (fun c -> c.name = s) l in
             Some (Printf.sprintf "%s: %s (prio %d)" c.name c.descr c.prio)
           with Not_found -> Some ("error: unknown command " ^ s))
      in
      Lwt.return res)

let run ~prefix core l msg : unit Lwt.t =
  let rec aux = function
    | [] ->
      Logs.debug (fun k ->
          k "no command found for %s" (Core.string_of_privmsg msg));
      Lwt.return_unit
    | c :: tail ->
      (match c.match_ ~prefix core msg with
      | Cmd_skip -> aux tail
      | Cmd_match f ->
        Logs.debug (fun k ->
            k "command %s succeeded for %s" c.name (Core.string_of_privmsg msg));
        f
      | Cmd_fail e ->
        Logs.debug (fun k ->
            k "command %s failed on %s with %s" c.name
              (Core.string_of_privmsg msg)
              e);
        aux tail)
  in
  aux l
