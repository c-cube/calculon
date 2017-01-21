
(** {1 Lwt+Result Monad} *)

type 'a t = ('a,string) Result.result Lwt.t

let return x = Lwt.return (Ok x)
let fail e = Lwt.return (Error e)

let lift : ('a,string) Result.result -> 'a t = Lwt.return
let ok : 'a Lwt.t -> 'a t = fun x -> Lwt.map (fun y -> Ok y) x

let (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  = fun e f ->
    Lwt.bind e (function
      | Error e -> Lwt.return (Error e)
      | Ok x -> f x
    )

let (>>?=) : 'a t -> ('a -> ('b, string) Result.result) -> 'b t
  = fun e f ->
    Lwt.bind e (function
      | Error e -> Lwt.return (Error e)
      | Ok x -> f x |> Lwt.return
    )

let (>|=) : 'a t -> ('a -> 'b) -> 'b t
  = fun e f ->
    Lwt.map (function
      | Error e -> Error e
      | Ok x -> Ok (f x)
    ) e

let rec map_s : ('a -> 'b t) -> 'a list -> 'b list t
  = fun f l -> match l with
    | [] -> return []
    | x :: tail ->
      f x >>= fun x' -> map_s f tail >|= fun tail' -> x' :: tail'

let map_err f =
  Lwt.map (function
    | Result.Ok x -> Result.Ok x
    | Result.Error y -> Result.Error (f y))
