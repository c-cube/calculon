all: build test

build:
	@dune build @all

install:
	@dune install

test: build
	@dune runtest --no-buffer --force

clean:
	@dune clean

format:
	@dune fmt --auto-promote

WATCH?=@check
watch:
	@dune build -w $(WATCH)

doc:
	@dune build @doc

VERSION=$(shell awk '/^version:/ {print $$2}' calculon.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)

.PHONY: format clean
