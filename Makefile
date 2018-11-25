all: build test

build:
	dune build @install

install:
	dune install

test: build
	dune runtest --no-buffer --force

clean:
	dune clean

doc:
	dune build @doc

