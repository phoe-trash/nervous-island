;;;; nervous-island.asd

(asdf:defsystem #:nervous-island
  :description "ネウロ島六角ボードゲームエンジン"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :pathname "src"
  :components ((:file "grid")
               (:module "tiles"
                :components ((:file "army")
                             (:file "tile")
                             (:file "instant")
                             (:file "skill")
                             (:file "attack")
                             (:file "effect")))
               (:module "armies"
                :components ((:file "moloch")
                             (:file "outpost")
                             (:file "borgo")
                             (:file "hegemony")))
               (:file "user"))
  :depends-on (#:alexandria
               #:protest/base))
