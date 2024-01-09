(in-package :asdf-user)

(defsystem "alien-game-of-life"
  :source-control (:git "https://github.com/certainty/alien-game-of-life.git")
  :version "0.1"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :maintainer "David Krentzlin <david.krentzlin@gmail.com>"
  :licence "BSD"
  :depends-on ("alexandria" "serapeum")
  :entry-point "agol:main"
  :serial t
  :pathname "src"
  :components
  ((:file "packages")
   (:file "settings")
   (:module "core"
    :components
    ((:file "state")))
   (:module "frontend"
    :components
    ((:file "console")))
   (:file "main")))
