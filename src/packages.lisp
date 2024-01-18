(in-package :cl-user)

(defpackage :agol
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   :run-game
   :main))

(defpackage :agol.frontend.console
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   :draw
   :draw-grid))
