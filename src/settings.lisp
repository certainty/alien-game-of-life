(in-package :agol)

(defparameter *max-generations* nil "nil means infinite")
(defparameter *grid-rows* 30)
(defparameter *grid-columns* 100)
(defparameter *grid-wraps-around-p* t "if true, the grid wraps around at the edges")
(defparameter *stop-when-stable-p* t "if true, stop when the game is stable")
(defparameter *stable-threshold* 10 "how many generations to wait before stopping")
(defparameter *mutation-rate* 0.0)
(defparameter *enable-colors-p* t)
(defparameter *frontend* 'console "the frontend to use")
(defparameter *sleep-time* 0.08 "time to sleep between generations")
