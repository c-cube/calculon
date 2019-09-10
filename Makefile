all: build test

build:
	@dune build @all

install:
	@dune install

test: build
	@dune runtest --no-buffer --force

clean:
	@dune clean

watch:
	@dune build @all --watch

doc:
	@dune build @doc

