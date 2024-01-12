.PHONY: run

run:
	sbcl --eval '(progn (ql:quickload :alien-game-of-life))' --eval '(agol:run-game)' --quit
