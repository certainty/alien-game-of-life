(in-package :cl-user)

(defpackage :agol
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   :draw
   :run-game
   :main))

(defpackage :agol.frontend.console
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   :draw-grid))
