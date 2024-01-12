(in-package :agol)

(defparameter *stop-game* nil)

(defun make-game ()
  (agol.core:initial-state
   *grid-rows*
   *grid-columns*
   :stop-when-stable-p *stop-when-stable-p*
   :grid-wraps-around-p *grid-wraps-around-p*
   :enable-colors-p *enable-colors-p*))

(defun run-game ()
  (let ((game (make-game))
        (current-generation 0)
        (ticks-without-change 0))
    (loop
      (when *stop-game*
        (return))

      (when (and *max-generations* (> current-generation *max-generations*))
        (return))

      (unless (tick game)
        (incf ticks-without-change))

      (incf current-generation)

      (when (and *stop-when-stable-p* (>= ticks-without-change *stop-when-stable-p*))
        (return)))))

(defun tick (game)
  (draw game)

  (when *sleep-time*
    (sleep *sleep-time*))

  (multiple-value-bind (state changed-cell-count) (agol.core::next-generation game)
    (declare (ignore state))
    (s:true (plusp changed-cell-count))))

(defun draw (game)
  (agol.frontend.console::draw game))
