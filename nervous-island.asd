;;;; nervous-island.asd

(asdf:defsystem #:nervous-island
  :description "ネウロ島六角ボードゲームエンジン"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "grid")
               (:file "army")
               (:file "tile")
               (:file "instant")
               (:file "skill")
               (:file "attack")
               (:file "effect")
               (:file "armies/outpost")
               (:file "armies/moloch")
               (:file "armies/borgo")
               (:file "armies/hegemony")
               (:file "user"))
  :depends-on (#:alexandria
               #:protest/base))
