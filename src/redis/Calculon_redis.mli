
val make_plugin :
  ?prefix:string ->
  ?host:string ->
  ?port:int ->
  unit -> Plugin.t
(** Create a plugin that will connect to a redis instance
    on the given [host:port] and use [prefix:received] and [prefix:send]
    for communicating messages via redis pub/sub.
    @param prefix (default "irc") prefix for channel names
*)

