all: build test

build:
	jbuilder build @install

test: build
	jbuilder runtest --no-buffer --force

clean:
	jbuilder clean

doc:
	jbuilder build @doc

