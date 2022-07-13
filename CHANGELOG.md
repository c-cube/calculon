
## 0.8

- add opam lockfiles
- check channel in !tell
- config: ability to join multiple channels
- remove ISO8601, use ptime instead
- use sqlite3 for storage
  * small `migrate_state` tool to produce a db file from state.json
- use github actions
- remove `tls_cert` config option
- factoid: add `max_cardinal_for_force`
- add custom commands (no prefix matching at all)
- count years in !seen

## 0.7

- add emoji plugin
- move to irc-client 0.7 with SASL support
- default to libera.chat

## 0.6

- use `logs` for logging, deprecate custom logger
- better irclogs parser
- use `Yojson.t`
- add `extra_args` to config parser

## 0.5

- refactor: use curly for the web plugin
- refactor: use irc-client-lwt-ssl instead of tls
- chore: make examples/tools native only
- fix `prefix1-full`
- feat: redis interface
- make calculon-extra non-optional

## 0.4

- refactor: pass prefix during matching, not at command creation
- test: use mdx, update readme to compile again
- chore: bump minimal OCaml version to 4.03
- refactor and fix `help` command
- chore: move to dune, including for demo bot
- update documentation
- add travis-ci
- migrate opam file to opam 2.0
- update doc of cmd_help, of_cmd, of_cmds
- add custom prefix for commands
- add command prefix in the description
- TLS client cert support

