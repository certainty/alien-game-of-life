(in-package :agol)

(defparameter *game* nil "the current game")

(defun make-game ()
  (setf *game* (agol.core:initial-state *grid-width* *grid-height*))
  *game*)

(defun run-game ()
  (make-game)
  (if *max-generations*
      (dotimes (i *max-generations*)
        (tick *game*))
      (loop (tick *game*))))

(defun tick (game)
  (format t "~%~%Generation ~a~%~%" i)
  (draw game)
  (agol.core::next-generation game))

(defun draw (game)
  (agol.frontend.console::draw game))
