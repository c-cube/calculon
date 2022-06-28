module Maybe = struct
  type t = string option

  let wrap = function
    | "N/A" -> None
    | s -> Some s

  let unwrap = Option.value ~default:"N/A"
end

module Year = struct
  type t = int option

  let wrap s =
    match Stringext.split ~max:3 ~on:' ' (String.trim s) with
    | [ _; _; year ] -> (try Some (int_of_string year) with _ -> None)
    | _ -> None

  let unwrap = Option.fold ~none:"N/A" ~some:(Printf.sprintf "1 Jan %d")
end
