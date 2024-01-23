(in-package :agol.frontend.console)

(setf cl-ansi-text:*color-mode* :24bit)
(s:defconst +alive-cell+ "▇")
(s:defconst +dead-cell+ " ")

(defclass console ()
  ((sprite-cache
    :initform (make-hash-table :test #'equalp))))

(defun make-console ()
  (make-instance 'console))

(defgeneric draw (frontend state)
  (:documentation "Draws the state to the frontend"))

(defmethod draw ((frontend console) state)
  (with-slots (sprite-cache) frontend
    (clear-screen)
    (draw-grid (agol::live-grid state) sprite-cache (agol::enable-colors-p state))
    (format t "~%Generation: ~a Max-Generations: ~a Live-Cells: ~a Colors: ~a~%" (agol::generation state) (or agol::*max-generations* "Infinity") (agol::live-cells state) (yes-no (agol::enable-colors-p state)))))

(defun draw-grid (grid sprite-cache &optional (colors-enabled-p nil))
  (let ((current-row 0))
    (agol::do-cells ((cell row column) grid)
      (unless (= current-row row)
        (terpri)
        (setf current-row row))
      (cond
        ((and (agol::cell-alive-p cell) colors-enabled-p)
         (a:if-let ((sprite (gethash (agol::cell-color cell) sprite-cache)))
           (princ sprite)
           (let ((sprite (render-colored-cell cell)))
             (setf (gethash (agol::cell-color cell) sprite-cache) sprite)
             (princ sprite))))
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
