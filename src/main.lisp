(in-package :agol)

(defparameter *game* nil)

(defun main (&key (generations 1) (width 30) (height 10))
  (setf *game* (agol.core:initial-state width height))
  (dotimes (i generations)
    (format t "~%~%Generation ~a~%~%" i)
    (agol.frontend.console::draw *game*)
    (agol.core::next-generation *game*)))
