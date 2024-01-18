(in-package :agol)

(defparameter *sleep-time* 0.08 "time to sleep between generations")
(defparameter *max-generations* nil "nil means infinite")
(defparameter *grid-rows* 30)
(defparameter *grid-columns* 100)
(defparameter *grid-wraps-around-p* t "if true, the grid wraps around at the edges")
(defparameter *enable-colors-p* t)

(defparameter *frontend* 'console "the frontend to use")
