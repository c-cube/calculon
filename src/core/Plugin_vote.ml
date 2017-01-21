open Result
open Lwt.Infix

(* TODO add proper lib *)
module Time = struct
  type t = float

  let minutes x = (float_of_int x) *. 60.

  let as_mins x = int_of_float @@ x /. 60.

  let display_mins x =
    match as_mins x with
    | 0 -> "less than 1 minute"
    | 1 -> "one minute"
    | n -> Printf.sprintf "%d minutes" n

  let now = Unix.gettimeofday
end

module Vote = struct
  type vote = For | Against

  type t = {
    purpose : string; (* short description *)
    expire : float; (* time at which the poll expires *)
    status : (string, vote) Hashtbl.t; (* nick -> vote *)
    mutable quorum : int option; (* how many votes needed to reach quorum? *)
  }

  let start ?quorum ?(duration=Time.minutes 30) purpose =
    { purpose; status = Hashtbl.create 10;
      expire = Time.(now () +. duration); quorum }

  let add_vote t nick vote =
    match Hashtbl.find t.status nick with
    | exception Not_found -> Hashtbl.add t.status nick vote
    | old_vote when old_vote = vote -> ()
    | _ -> Hashtbl.replace t.status nick vote

  (* results for one poll *)
  type result = {
    for_ : int;
    against: int;
  }

  let count_votes t : result =
    Hashtbl.fold
      (fun _ vote r -> match vote with
         | For -> {r with for_=r.for_+1}
         | Against -> {r with against=r.against+1})
      t.status
      { for_=0; against=0 }

  let vote_status t nick =
    try Some (Hashtbl.find t.status nick) with Not_found -> None

  let explain { purpose; _ } = purpose

  let show_status t =
    let r = count_votes t in
    Printf.sprintf "%s : expressed %d / for %d / against %d (expires in %s)"
      (explain t) (Hashtbl.length t.status) r.for_ r.against
      Time.(display_mins @@ t.expire -. now ())

  let missing_votes t : int option =
    match t.quorum with
      | None -> None
      | Some n -> Some (max 0 (n - Hashtbl.length t.status))

  let expired now { expire; _ } = expire < now

  let is_complete t =
    begin match missing_votes t with
      | Some 0 -> true
      | _ -> false
    end
    ||
    expired (Time.now ()) t

  let get_winner t =
    let r = count_votes t in
    if r.for_ > r.against then Some For
    else if r.against < r.for_ then Some Against
    else None

  let string_of_vote = function
    | For -> "for"
    | Against -> "against"

  let vote_of_string = function
    | "for" -> Ok For
    | "against" -> Ok Against
    | _ -> Error "wrong vote (expected 'for' or 'against')"
end

type poll = { creator : string; vote : Vote.t }

type state = (string, poll) Hashtbl.t

let max_polls_per_nick = 1
let max_polls = 5

let nb_polls_per_nick polls nick =
  Hashtbl.fold
    begin fun _ { creator;_ } count -> if creator = nick then count + 1 else count end
    polls 0

let show_status name { creator; vote } =
  Printf.sprintf "poll %s created by %s : %s"
    name creator (Vote.show_status vote)

let create_poll polls nick name purpose =
  match Hashtbl.length polls with
  | cur_len when cur_len >= max_polls -> Error "too many active polls"
  | _ ->
    match nb_polls_per_nick polls nick with
      | cur_polls when cur_polls >= max_polls_per_nick ->
        Error
          (Printf.sprintf "cannot create more than %d polls simultaneously"
             max_polls_per_nick)
      | _ ->
        match Hashtbl.find polls name with
          | poll -> Error (show_status name poll)
          | exception Not_found ->
            Hashtbl.add polls name { creator = nick; vote = Vote.start purpose };
            Ok None

let vote polls nick name vote =
  match Hashtbl.find polls name with
  | exception Not_found -> Error (Printf.sprintf "no such poll '%s'" name)
  | poll ->
    match Vote.vote_of_string vote with
    | Error _ as e -> e
    | Ok vote ->
      Vote.add_vote poll.vote nick vote;
      match Vote.is_complete poll.vote with
      | true ->
        Hashtbl.remove polls name;
        Ok (Some (Printf.sprintf "poll done : result %s"
              (CCOpt.get "draw" @@ CCOpt.map Vote.string_of_vote
               @@ Vote.get_winner poll.vote)))
      | _ -> Ok (Some (Vote.show_status poll.vote))

let show_vote polls name nick =
  match Hashtbl.find polls name with
    | exception Not_found ->
      Error (Printf.sprintf "no such active poll '%s'" name)
    | poll ->
      let vote =
        CCOpt.get "draw"
        @@ CCOpt.map Vote.string_of_vote
        @@ Vote.vote_status poll.vote nick
      in
      Ok (Some (Printf.sprintf "%s is %s %s" nick vote name))

let vote_status polls name =
  match Hashtbl.find polls name with
  | exception Not_found -> Error (Printf.sprintf "no active poll '%s'" name)
  | poll ->
    Ok (Some (show_status name poll))

let rec collector polls =
  let now = Time.now () in
  Hashtbl.iter
    begin fun name { vote; _ } ->
      if Vote.expired now vote then Hashtbl.remove polls name end
    polls;
  Lwt_unix.sleep (Time.minutes 1) >>= fun () ->
  collector polls

let init _core _conf : state Lwt.t =
  let polls = Hashtbl.create 10 in
  Lwt.async (fun () -> collector polls);
  Lwt.return polls

let help =
  "!vote show <poll> <nick> : display current vote of <nick> for <poll>\n\
   !vote start <poll> <description (optional)> : create new poll\n\
   !vote status <poll> : display current status of <poll>\n\
   !vote for <poll> : vote for the given <poll>\n\
   !vote against <poll>: vote against the given <poll>\n\
  "

let reply polls msg s =
  let reply_res = function
    | Error msg ->
      let message = Printf.sprintf "%s: %s" Talk.(select Err) msg in
      Some message |> Lwt.return
    | Ok x -> x |> Lwt.return
  in
  begin match Stringext.split ~max:3 (String.trim s) ~on:' ' with
    | "show" :: name :: nick :: _ ->
      show_vote polls name nick |> reply_res
    | "start" :: name :: purpose ->
      create_poll polls msg.Core.nick name
        (match purpose with [] -> "" | purpose :: _ -> purpose)
      |> reply_res
    | "status" :: name :: _ -> vote_status polls name |> reply_res
    | ("pour" | "contre" as v) :: name :: _ ->
      vote polls msg.Core.nick name v |> reply_res
    | _ -> Error "what did you say ?" |> reply_res
  end

let cmd_vote state : Command.t =
  Command.make_simple
    ~descr:("vote system for yes/no questions\n" ^ help)
    ~prefix:"vote" ~prio:10
    (reply state)

let plugin =
  Plugin.stateful
    ~init
    ~stop:(fun _ -> Lwt.return_unit)
    (fun state -> [cmd_vote state])

