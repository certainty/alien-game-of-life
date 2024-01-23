(in-package :asdf-user)

(defsystem "alien-game-of-life"
  :source-control (:git "https://github.com/certainty/alien-game-of-life.git")
  :version "0.1"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :maintainer "David Krentzlin <david.krentzlin@gmail.com>"
  :licence "BSD"
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "agol"
  :depends-on ("alexandria" "serapeum" "cl-ansi-text" "cl-colors2")
  :entry-point "agol::main"
  :serial t
  :pathname "src"
  :components
  ((:file "packages")
   (:file "settings")
   (:file "state")
   (:file "frontend")
   (:file "console")
   (:file "main")))
