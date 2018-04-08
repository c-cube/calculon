open Prelude
open Plugin

(** Separate a string into two parts using the given separator **)
let extract_choices sep str =
  let rec list_to_string l =
    match l with
      | s::ss -> s ^ " " ^ list_to_string ss
      | [] -> ""
  in
  let rec remove_empty_strings l =
    match l with
    | [] -> []
    | ""::ss -> remove_empty_strings ss
    | s::ss -> s::(remove_empty_strings ss)
  in
  let rec split_list_on_element equal el l =
    match l with
      | e::es ->
          if equal e el then
            ([], es)
          else
            let (l1, l2) = split_list_on_element equal el es in
            (e::l1, l2)
      | [] -> ([], [])
  in
  let map_couple f (a,b) = (f a, f b) in

  match String.split_on_char ' ' str |> remove_empty_strings with
    | [] -> None
    | [_] -> None
    | [s1;s2] -> Some (s1, s2)
    | l -> (match split_list_on_element String.equal sep l |> map_couple list_to_string with
      | ("",_) -> None
      | (_,"") -> None
      | c -> Some c)

(** Make several successives separator attempts on the string **)
let rec extract_choices_seps sep_list str =
  match sep_list with
  | [] -> None
  | s::ss -> (
    match extract_choices s str with
      | None -> extract_choices_seps ss str
      | Some c -> Some c)

let cmd_choice =
  let list_of_couple (a,b) = [a; b] in
  let supported_separators = ["vs"; "|"; "||"; "&"; "&&"; "or"; "and"; "ou"; "et";
    "contre"; "versus"] in
  let cmd_function =
    (fun _ str ->
      let choice_opt =  extract_choices_seps supported_separators
        (str |> String.lowercase_ascii |> String.trim)
        |> map_opt list_of_couple |> map_opt random_l in
      let msg = choice_opt
        |> map_opt (fun s -> "Et c'est décidé ! Ce sera " ^ (String.uppercase_ascii s))
        |? "Malheureusement, il n'est pas possible de parser votre indécision, même avec les dernières percées de nos IA multi-cloud-ready bare metal. Faîtes un effort !"
      in
      Lwt.return (some msg)
    )
  in
  Command.make_simple
    ~descr:"Faire un choix entre deux options"
    ~prefix:"choix"
    ~prio:10
    cmd_function

let plugin = of_cmd cmd_choice
