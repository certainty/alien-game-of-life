(in-package :agol)

(defparameter *stop-game* nil)

(defun configure-from-env ()
  (let ((rows (parse-integer (or (uiop:getenv "AGOL_ROWS") "") :junk-allowed t))
        (cols (parse-integer (or (uiop:getenv "AGOL_COLS") "") :junk-allowed t))
        (max-generations (parse-integer (or (uiop:getenv "AGOL_MAX_GENERATIONS") "") :junk-allowed t))
        (disable-colors (uiop:getenv "AGOL_DISABLE_COLORS")))
    (when rows
      (setf *grid-rows* rows))
    (when cols
      (setf *grid-columns* cols))
    (when max-generations
      (setf *max-generations* max-generations))
    (when disable-colors
      (setf *enable-colors-p* nil))))

(defun main ()
  (configure-from-env)
  (handler-case (run-game)
    (sb-sys:interactive-interrupt ()
      (format t "~%Bye!~%")
      (sb-ext:quit))))

(defun make-game ()
  (make-state
   *grid-rows*
   *grid-columns*
   :grid-wraps-around-p *grid-wraps-around-p*
   :enable-colors-p *enable-colors-p*))

(defun run-game ()
  (let* ((game (make-game))
         (current-generation 0)
         (ticks-without-change 0)
         (console (agol.frontend.console::make-console)))
    (loop
      (when (or *stop-game* (and *max-generations* (> current-generation *max-generations*)))
        (return))
      (unless (tick console game)
        (incf ticks-without-change))
      (incf current-generation))))

(defun tick (console game)
  (agol.frontend.console::draw console game)
  (when *sleep-time*
    (sleep *sleep-time*))
  (plusp (next-generation game)))

