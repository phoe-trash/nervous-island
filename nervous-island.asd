;;;; nervous-island.asd

(asdf:defsystem #:nervous-island
  :description "ネウロ島六角ボードゲームエンジン"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :pathname "src"
  :components ((:file "common")
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
               (:module "state"
                :components ((:file "coord")
                             (:file "board")
                             (:file "space")
                             (:file "player")
                             (:file "phase")
                             (:file "choice")
                             (:file "state")))
               (:file "user"))
  :depends-on (#:alexandria
               #:protest/base)
  :in-order-to ((test-op (load-op :nervous-island.test)))
  :perform
  (test-op (o c)
           (symbol-call '#:parachute '#:test :nervous-island.test
                        :report (find-symbol "INTERACTIVE" "PARACHUTE"))))
