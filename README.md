
# Calculon

Library for writing IRC bots in OCaml, a collection of plugins, and a dramatic robotic actor.
The core library is called `calculon`.

## Build

```
make build
```

## Introduction to the Code

### Main

The typical `main` entry point would look like  this.
Calculon works by gathering a list of
*plugins* (see the module `Plugin`), some configuration (see `Config`)
and running the package in a loop using [irc-client](https://github.com/johnelse/ocaml-irc-client/).

```ocaml

module C = Calculon

let plugins : C.Plugin.t list = [
  C.Plugin_social.plugin;
  C.Plugin_factoids.plugin;
  my_own_plugin;
  (* etc. *)
]

let () =
  let conf = C.Config.of_argv () in
  C.Run_main.main conf plugins |> Lwt_main.run


```

### Plugins

A plugin contains some hooks for startup and teardown
(mostly for storing/loading state on disk) and a set of *commands*.
Typically a command (see the module `Command`) is a rule that matches a IRC
message with some regex, and decides whether or not to fire with a reply.

For instance, the following module will reply to messages
starting with `!hello` by replying `"hello <sender>"`. This is a simple
command, as the function `Command.make_simple` indicates: it returns a `string
option` to indicate whether or not to respond to any line starting with
`!prefix`. More elaborate commands are possible using `Command.make`.

```ocaml

let my_own_plugin : Command.t =
  Command.make_simple ~descr:"hello world" ~prefix:"hello" ~prio:10
    (fun (input_msg:Core.privmsg) _ ->
       let who = input_msg.Core.nick in
       Lwt.return (Some ("hello " ^ who))
    )

let plugin = Plugin.of_cmd cmd_hello
```

See the existing plugins in `Factoids` and `Social` to see how to implement
stateful plugins that store their data on disk.
