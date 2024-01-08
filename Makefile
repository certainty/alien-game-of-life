.PHONY: run

run:
	ros run -Q -e '(ql:quickload :alien-game-of-life)' -e '(agol:main)'
