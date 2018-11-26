
# Calculon [![Build Status](https://travis-ci.org/c-cube/calculon.svg?branch=master)](https://travis-ci.org/c-cube/calculon)

Library for writing IRC bots in OCaml, a collection of plugins, and a dramatic robotic actor.
The core library is called `calculon`.

![calculon logo](https://raw.github.com/c-cube/calculon/master/media/calculon.jpg)

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

A plugin contains a set of *commands*.
A command is is a rule that matches a IRC message with some regex, and decides
whether or not to fire with a reply. They are defined in the module `Command`.

For instance, the following module will reply to messages
starting with `!hello` by replying `"hello <sender>"`. This is a simple
command, as the function `Command.make_simple` indicates: it returns a `string
option` to indicate whether or not to respond to any line starting with
`!prefix`. More elaborate commands are possible using `Command.make`.

```ocaml

let cmd_hello : Command.t =
  Command.make_simple ~descr:"hello world" ~prefix:"hello" ~prio:10
    (fun (input_msg:Core.privmsg) _ ->
       let who = input_msg.Core.nick in
       Lwt.return (Some ("hello " ^ who))
    )

let plugin_hello = Plugin.of_cmd cmd_hello
```

Basic plugins are stateless, built from one or more commands with `Plugin.of_cmd`
and `Plugin.of_cmds`.
Other plugins can be stateful (typically, they can have some persistent
state, or more "custom" schemes).
The constructor `Plugin.stateful` is used to make such plugins.
All the persistent state is stored in a single json file.

See for instance the existing plugins `Plugin_factoids` and `Plugin_movie`
to see how to use `Plugin.stateful`.
