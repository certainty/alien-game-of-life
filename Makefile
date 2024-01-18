.PHONY: run build

LISP ?= sbcl --noinform

run: build
	./bin/agol

build:
	$(LISP) --eval '(ql:quickload :alien-game-of-life)' --eval '(asdf:make :alien-game-of-life)' --quit
