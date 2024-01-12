(in-package :agol.frontend.console)

(defun draw (state)
  (clear-screen)
  (format t "Generation: ~a Max-Generations: ~a Live-Cells: ~a Stop when stable: ~a~%"
          (agol.core:generation state)
          agol::*max-generations*
          (agol.core:live-cells state)
          (agol.core:stop-when-stable-p state))
  (draw-grid (agol.core:live-grid state)))

(defun draw-grid (grid)
  (let ((current-row 0))
    (agol.core::do-cells ((cells row column) grid)
      (unless (= current-row row)
        (terpri)
        (setf current-row row))
      ;; TODO: optimize the IO here
      (format t "~a" (if (agol.core:cell-alive-p (aref cells row column)) "▇" " ")))))

(defun clear-screen ()
  "Clears the screen and sets the cursor to the top left."
  (format t "~c[2J" #\escape)
  (format t "~c[H" #\escape))
