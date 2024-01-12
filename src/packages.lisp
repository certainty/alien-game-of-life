(in-package :cl-user)

(defpackage :agol
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   :*game*
   :*max-generations*
   :*grid-width*
   :*grid-height*
   :*grid-wraps-around-p*
   :*enable-colors-p*
   :*mutation-rate*
   :*frontend*
   :*stop-when-stable-p*
   :tick
   :run-game))

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
   :generation
   :live-cells
   :stop-when-stable-p
   :next-generation))

(defpackage :agol.frontend.console
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   :draw
   :draw-grid))
