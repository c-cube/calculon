
(** {1 Simple Runner} *)

val main : Config.t -> Plugin.t list -> unit Lwt.t
(** Easy wrapper for a bot that takes some configuration
    (including the connection options, such as the network
    and channel to join) and a list of plugins, and
    runs the (re)connection loop with the list
    of plugins.

    Main should look like this:
    {[
      let main () =
        let module C = Calculon in
        let config = C.Config.parse C.Config.default Sys.argv in
        C.Run_main.main config plugins |> Lwt_main.run
    ]}
*)

