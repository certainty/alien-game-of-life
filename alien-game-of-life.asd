(in-package :asdf-user)

(defsystem "alien-game-of-life"
  :source-control (:git "https://github.com/certainty/alien-game-of-life.git")
  :version "0.1"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :maintainer "David Krentzlin <david.krentzlin@gmail.com>"
  :licence "BSD"
  :depends-on ("alexandria" "serapeum")
  :serial t
  :pathname "src"
  :components
  ((:file "packages")
   (:module "core"
    :components
    ((:file "state")))
   (:module "frontend")))
