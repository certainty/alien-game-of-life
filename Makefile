.PHONY: run build

run: build
	./bin/agol

build:
	sbcl --eval '(ql:quickload :alien-game-of-life)' --eval '(asdf:make :alien-game-of-life)' --quit
