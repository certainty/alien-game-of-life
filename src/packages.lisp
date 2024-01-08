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
   :state
   :do-cells))

(defpackage :agol.frontend.console
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->))
