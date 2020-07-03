;;;; nervous-island.asd

(asdf:defsystem #:nervous-island
  :description "ネウロ島六角ボードゲームエンジン"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "grid"))
  :depends-on (#:alexandria
               #:protest/base))
