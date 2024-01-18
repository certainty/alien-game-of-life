(in-package :agol.frontend.console)

(setf cl-ansi-text:*color-mode* :24bit)

(s:defconst +alive-cell+ "▇")
(s:defconst +dead-cell+ " ")

(defun make-sprite-map ()
  (let ((map (make-hash-table :test #'equal))
        (mappings `((,agol.core::+color-yellow+ . ,cl-colors2:+yellow+)
                    (,agol.core::+color-green+ . ,cl-colors2:+green+)
                    (,agol.core::+color-blue+ . ,cl-colors2:+blue+)
                    (,agol.core::+color-red-violet+ . ,cl-colors2:+violet-red+)
                    (,agol.core::+color-violet+ . ,cl-colors2:+violet+)
                    (,agol.core::+color-red+ . ,cl-colors2:+red+)
                    (,agol.core::+color-orange+ . ,cl-colors2:+orange+)
                    (,agol.core::+color-yellow-green+ . ,cl-colors2:+yellow-green+))))
    (prog1 map
      (dolist (mapping mappings)
        (setf (gethash (car mapping) map)
              (with-output-to-string (s)
                (cl-ansi-text:with-color ((cdr mapping) :stream s) (format s "~a" +alive-cell+))))))))

(defparameter *default-sprite-map* (make-sprite-map))

(defun draw (state)
  (clear-screen)
  (format t "Generation: ~a Max-Generations: ~a Live-Cells: ~a Colors: ~a Stop when stable: ~a~%"
          (agol.core:generation state)
          (or agol::*max-generations* "Infinity")
          (agol.core:live-cells state)
          (yes-no (agol.core::enable-colors-p state))
          (yes-no (agol.core::stop-when-stable-p state)))
  (draw-grid (agol.core:live-grid state) (agol.core::enable-colors-p state)))

(defun draw-grid (grid &optional (colors-enabled-p nil))
  (let ((current-row 0))
    (agol.core::do-cells ((cells row column) grid)
      (unless (= current-row row)
        (terpri)
        (setf current-row row))
      (let ((cell (aref cells row column)))
        (cond
          ((and (agol.core:cell-alive-p cell) colors-enabled-p)
           (let ((sprite (gethash (agol.core:cell-color cell) *default-sprite-map*)))
             (princ sprite)))
          ((agol.core:cell-alive-p cell)
           (princ +alive-cell+))
          (t (princ +dead-cell+)))))))

(defun clear-screen ()
  "Clears the screen and sets the cursor to the top left."
  (format t "~c[2J" #\escape)
  (format t "~c[H" #\escape))

(defun yes-no (b)
  (if b "yes" "no"))
