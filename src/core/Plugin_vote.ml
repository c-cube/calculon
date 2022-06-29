open Result
open Lwt_infix

(* TODO add proper lib *)
module Time = struct
  type t = float

  let minutes x : t = float_of_int x *. 60.
  let as_mins x = int_of_float @@ (x /. 60.)

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
    purpose: string; (* short description *)
    expire: float; (* time at which the poll expires *)
    status: (string, vote) Hashtbl.t; (* nick -> vote *)
    mutable quorum: int option; (* how many votes needed to reach quorum? *)
  }

  let start ?quorum ?(duration = Time.minutes 30) purpose =
    {
      purpose;
      status = Hashtbl.create 10;
      expire = Time.(now () +. duration);
      quorum;
    }

  let add_vote t nick vote =
    match CCHashtbl.get t.status nick with
    | None -> Hashtbl.add t.status nick vote
    | Some old_vote when old_vote = vote -> ()
    | _ -> Hashtbl.replace t.status nick vote

  (* results for one poll *)
  type result = { for_: int; against: int }

  let count_votes t : result =
    Hashtbl.fold
      (fun _ vote r ->
        match vote with
        | For -> { r with for_ = r.for_ + 1 }
        | Against -> { r with against = r.against + 1 })
      t.status { for_ = 0; against = 0 }

  let vote_status t nick =
    try Some (Hashtbl.find t.status nick) with Not_found -> None

  let show_status t =
    let r = count_votes t in
    Printf.sprintf "expressed %d / for %d / against %d (expires in %s)"
      (Hashtbl.length t.status) r.for_ r.against
      Time.(display_mins @@ (t.expire -. now ()))

  let missing_votes t : int option =
    match t.quorum with
    | None -> None
    | Some n -> Some (max 0 (n - Hashtbl.length t.status))

  let expired now { expire; _ } = expire < now

  let is_complete t =
    (match missing_votes t with
    | Some 0 -> true
    | _ -> false)
    || expired (Time.now ()) t

  let get_winner t =
    let r = count_votes t in
    if r.for_ > r.against then
      Some For
    else if r.against < r.for_ then
      Some Against
    else
      None

  let string_of_vote = function
    | For -> "for"
    | Against -> "against"

  let vote_of_string = function
    | "for" -> Ok For
    | "against" -> Ok Against
    | _ -> Error "wrong vote (expected 'for' or 'against')"
end

type poll = { purpose: string; creator: string; vote: Vote.t }
type state = { polls: (string, poll) Hashtbl.t; mutable stop: bool }

let max_polls_per_nick = 1
let max_polls = 5

let nb_polls_per_nick polls nick =
  Hashtbl.fold
    (fun _ { creator; _ } count ->
      if creator = nick then
        count + 1
      else
        count)
    polls 0

let show_status name { creator; vote; _ } =
  Printf.sprintf "Poll %s (created by %s) : %s" name creator
    (Vote.show_status vote)

let create_poll polls nick name purpose =
  match Hashtbl.length polls with
  | cur_len when cur_len >= max_polls ->
    Error
      "cannot create a new poll: max number has been reached, please delete \
       one before proceeding"
  | _ ->
    (match nb_polls_per_nick polls nick with
    | cur_polls when cur_polls >= max_polls_per_nick ->
      Error
        (Printf.sprintf
           "cannot create a new poll: max number by user has been reached: %d, \
            please delete one before proceeding"
           max_polls_per_nick)
    | _ ->
      (match CCHashtbl.get polls name with
      | Some poll ->
        Error
          (Printf.sprintf "a poll already exists with this name: %s"
             (show_status name poll))
      | None ->
        let poll =
          { purpose = name; creator = nick; vote = Vote.start purpose }
        in
        Hashtbl.add polls name poll;
        Ok
          (Some
             (Printf.sprintf "Poll %s successfully created! %s" name
                (show_status name poll)))))

let vote polls nick name vote =
  match CCHashtbl.get polls name with
  | None -> Error (Printf.sprintf "no poll called '%s'" name)
  | Some poll ->
    (match Vote.vote_of_string vote with
    | Error _ as e -> e
    | Ok vote ->
      Vote.add_vote poll.vote nick vote;
      (match Vote.is_complete poll.vote with
      | true ->
        Hashtbl.remove polls name;
        Ok
          (Some
             (Printf.sprintf "Poll time has ended!: The final result is %s"
                (Option.value ~default:"draw"
                @@ Option.map Vote.string_of_vote
                @@ Vote.get_winner poll.vote)))
      | _ -> Ok (Some (Vote.show_status poll.vote))))

let show_vote polls name nick =
  match CCHashtbl.get polls name with
  | None -> Error (Printf.sprintf "no active poll named '%s'" name)
  | Some poll ->
    let vote =
      Option.value ~default:"draw"
      @@ Option.map Vote.string_of_vote
      @@ Vote.vote_status poll.vote nick
    in
    Ok (Some (Printf.sprintf "%s is %s %s" nick vote name))

let vote_status polls name =
  match CCHashtbl.get polls name with
  | None -> Error (Printf.sprintf "no active poll named '%s'" name)
  | Some poll -> Ok (Some (show_status name poll))

let rec collector (st : state) : _ Lwt.t =
  let now = Time.now () in
  Hashtbl.iter
    (fun name { vote; _ } ->
      if Vote.expired now vote then Hashtbl.remove st.polls name)
    st.polls;
  Lwt_unix.sleep (Time.minutes 1) >>= fun () -> collector st

let help =
  "!vote show <poll> <nick> : display current vote of <nick> for <poll>\n\
   !vote start <poll> <description (optional)> : create new poll\n\
   !vote status <poll> : display current status of <poll>\n\
   !vote for <poll> : vote for the given <poll>\n\
   !vote against <poll>: vote against the given <poll>\n"

let reply (self : state) msg s : _ Lwt.t =
  let message_usage =
    "Please use `!vote for VOTE_NAME` or `!vote against VOTE_NAME` to vote; or \
     start a new vote with `!vote start VOTE_NAME`. (run !help vote for the \
     complete list of commands)"
  in
  let reply_res = function
    | Error msg ->
      let message = Printf.sprintf "%s: %s" Talk.(select Err) msg in
      Lwt.return @@ Some message
    | Ok x -> Lwt.return x
  in
  match Stringext.split ~max:3 (String.trim s) ~on:' ' with
  | "show" :: name :: nick :: _ -> show_vote self.polls name nick |> reply_res
  | "start" :: name :: purpose ->
    create_poll self.polls msg.Core.nick name
      (match purpose with
      | [] -> ""
      | purpose :: _ -> purpose)
    |> reply_res
  | "status" :: name :: _ -> vote_status self.polls name |> reply_res
  | (("for" | "against") as v) :: name :: _ ->
    vote self.polls msg.Core.nick name v |> reply_res
  | [ (("show" | "start" | "status" | "for" | "against") as v) ] ->
    Error
      (Printf.sprintf
         "this command is missing the vote name. Please specify one as in \
          `vote %sVOTE_NAME"
         v)
    |> reply_res
  | _ -> Error ("invalid command. " ^ message_usage) |> reply_res

let cmd_vote state : Command.t =
  Command.make_simple
    ~descr:("vote system for yes/no questions\n" ^ help)
    ~cmd:"vote" ~prio:10 (reply state)

let of_json _ _ : (state, _) result =
  let polls = { stop = false; polls = Hashtbl.create 10 } in
  Lwt.async (fun () -> collector polls);
  Ok polls

let plugin =
  Plugin.stateful ~name:"vote"
    ~to_json:(fun _ -> None)
    ~of_json
    ~commands:(fun state -> [ cmd_vote state ])
    ~stop:(fun st ->
      st.stop <- true;
      Lwt.return ())
    ()
