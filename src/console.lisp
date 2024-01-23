(in-package :agol.frontend.console)

(setf cl-ansi-text:*color-mode* :24bit)
;;(s:defconst +alive-cell+ "▇")
;;(s:defconst +alive-cell+ "◉")
(s:defconst +alive-cell+ "●")
(s:defconst +dead-cell+ " ")

(defclass console () ())

(defun make-console ()
  (make-instance 'console))

(defmethod agol::draw ((frontend console) state)
  (clear-screen)
  (draw-grid (agol::live-grid state) (agol::enable-colors-p state))
  (format t
          "~%Generation: ~a Max-Generations: ~a Live-Cells: ~a Colors: ~a ~%"
          (agol::generation state)
          (or agol::*max-generations* "Infinity")
          (agol::live-cells state)
          (yes-no (agol::enable-colors-p state))))

(defun draw-grid (grid &optional (colors-enabled-p nil))
  (let ((current-row 0))
    (agol::do-cells ((cell row column) grid)
      (unless (= current-row row)
        (terpri)
        (setf current-row row))
      (cond
        ((and (agol::cell-alive-p cell) colors-enabled-p)
         (princ (render-colored-cell cell)))
        ((agol::cell-alive-p cell)
         (princ +alive-cell+))
        (t (princ +dead-cell+))))))

(defun render-colored-cell (cell)
  (with-output-to-string (s)
    (cl-ansi-text:with-color ((agol::cell-color cell) :stream s)
      (princ +alive-cell+ s))))

(defun clear-screen ()
  "Clears the screen and sets the cursor to the top left."
  (format t "~c[2J" #\escape)
  (format t "~c[H" #\escape))

(defun yes-no (b)
  (if b "yes" "no"))
