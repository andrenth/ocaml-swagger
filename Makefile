.PHONY:
	build clean

build:
	dune build

default:
	build

clean:
	dune clean

