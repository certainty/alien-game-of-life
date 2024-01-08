(in-package :cl-user)

(defpackage :agol
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export :main))

(defpackage :agol.core
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   :make-cell
   :cell-color
   :cell-alive-p
   :initial-state
   :state
   :do-cells
   :live-grid
   :next-generation))

(defpackage :agol.frontend.console
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   :draw
   :draw-grid))
