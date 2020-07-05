;;;; nervous-island.test.asd

(asdf:defsystem #:nervous-island.test
  :description "Test suite for nervous-island"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :pathname "test"
  :components ((:file "package")
               (:module "tiles"
                :components ((:file "army")
                             (:file "skill")
                             (:file "attack")
                             (:file "effect")
                             (:file "tile")
                             (:file "instant")
                             (:file "token")))
               (:module "state"
                :components ((:file "coord")
                             (:file "board")
                             (:file "space")
                             ;; (:file "player")
                             ;; (:file "phase")
                             ;; (:file "choice")
                             ;; (:file "state")
                             )))
  :depends-on (#:alexandria
               #:parachute
               #:named-readtables
               #:nervous-island)
  :perform
  (test-op (o c)
           (symbol-call '#:parachute '#:test :nervous-island.test
                        :report (find-symbol "INTERACTIVE" "PARACHUTE"))))
