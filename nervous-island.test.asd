;;;; nervous-island.test.asd

(asdf:defsystem #:nervous-island.test
  :description "Test suite for nervous-island"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :pathname "test"
  :components ((:file "package")
               (:file "coord")
               (:file "board"))
  :depends-on (#:alexandria
               #:parachute
               #:named-readtables
               #:nervous-island)
  :perform
  (test-op (o c)
           (symbol-call '#:parachute '#:test :nervous-island.test
                        :report (find-symbol "INTERACTIVE" "PARACHUTE"))))
