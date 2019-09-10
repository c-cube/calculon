
type t

val make :
  ?debug:bool ->
  ?prefix:string ->
  ?host:string ->
  ?port:int ->
  unit -> (t, string) result Lwt.t

val close : t -> unit Lwt.t

val received : t -> Message_t.message Lwt_stream.t

val on_received : t -> (Message_t.message -> unit Lwt.t) -> unit

val join : t -> unit Lwt.t

val send : t -> Message_t.send -> unit Lwt.t
