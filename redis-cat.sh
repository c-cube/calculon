#!/bin/sh

exec dune exec src/redis-cat/redis_cat.exe -- $@
