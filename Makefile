all: build

build:
	@dune build

run: build
	@dune exec ./src/main.exe

clean:
	@dune clean
