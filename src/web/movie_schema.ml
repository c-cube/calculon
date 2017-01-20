module Maybe = struct
  type t = string option

  let wrap = function
    | "N/A" -> None
    | s -> Some s

  let unwrap = CCOpt.get "N/A" 

end

module Year = struct
  type t = int option

  let wrap s =
    match Stringext.split ~max:3 ~on:' ' (String.trim s) with
    | _::_::year::[] -> begin try Some (int_of_string year) with _ -> None end
    | _ -> None

  let unwrap = CCOpt.map_or ~default:"N/A" (Printf.sprintf "1 Jan %d")
end

