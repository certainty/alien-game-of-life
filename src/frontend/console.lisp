(in-package :agol.frontend.console)

(defun draw (state)
  (clear-screen)
  (draw-grid (agol.core:live-grid state)))

(defun draw-grid (grid)
  (let ((row 0))
    (agol.core::do-cells ((cell x y) grid)
      (unless (= row y)
        (format t "~%")
        (setf row y))
      (format t "~a" (if (agol.core:cell-alive-p cell) "X" " ")))))

(defun clear-screen ()
  "Clears the screen and sets the cursor to the top left."
  (format t "~c[2J" #\escape)
  (format t "~c[H" #\escape))
