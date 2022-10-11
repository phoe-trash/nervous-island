;;;; nervous-island.asd

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUTO-MODULE - adapted from https://github.com/sjl/rosalind

(defclass auto-module (module)
  ((cachedp :initform nil)))

(defmethod component-children :before ((self auto-module))
  (unless (slot-value self 'cachedp)
    (flet ((make-file (pathname)
             (make-instance 'cl-source-file
                            :name (pathname-name pathname)
                            :type "lisp"
                            :pathname pathname
                            :parent (component-parent self))))
      (let* ((pattern (make-pathname :directory nil :name uiop:*wild*
                                     :type "lisp"))
             (pathnames (directory-files (component-pathname self) pattern))
             (children (mapcar #'make-file pathnames)))
        (setf (slot-value self 'asdf/component:children) children
              (slot-value self 'cachedp) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; System definitions

(asdf:defsystem #:nervous-island/common
  :description "ネウロ島六角ボードゲームエンジン"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:trivial-indent
               #:protest/base
               #:value-semantics-utils
               #:closer-mop)
  :pathname "src/common"
  :components ((:file "common")))

(asdf:defsystem #:nervous-island/tiles
  :description "ネウロ島六角ボードゲームエンジン"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :depends-on (#:nervous-island/common)
  :pathname "src/tiles"
  :components ((:file "element")
               (:file "skill")
               (:file "attack")
               (:file "effect")
               (:file "token")
               (:file "army")
               (:file "tile")))

(asdf:defsystem #:nervous-island/armies
  :description "ネウロ島六角ボードゲームエンジン"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial nil
  :depends-on (#:nervous-island/tiles)
  :pathname "src"
  :components ((auto-module "armies")))

(asdf:defsystem #:nervous-island/junk
  :description "ネウロ島六角ボードゲームエンジン"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :depends-on (;; NI dependencies
               #:nervous-island/common
               #:nervous-island/tiles
               ;; GUI dependencies
               ;; #:vecto
               ;; #:imago
               ;; #:vecto-imago
               )
  :pathname "src"
  :components (;; (:module "state"
               ;;  :components ((:file "coord")
               ;;               (:file "board")
               ;;               (:file "space")
               ;;               (:file "player")
               ;;               (:file "damage")
               ;;               (:file "phase")
               ;;               (:file "step")
               ;;               (:file "choice")
               ;;               (:file "state")))
               ;; (:module "gui"
               ;;  :componentsq
               ;;  ((:module "shapes"
               ;;    :components ((:file "package")
               ;;                 (:file "common")
               ;;                 (:file "attacks")
               ;;                 (:file "net")
               ;;                 (:file "armor")
               ;;                 (:file "circle")
               ;;                 (:file "text")
               ;;                 (:file "mobility")
               ;;                 (:file "toughness")
               ;;                 (:file "bomb")
               ;;                 (:file "module")))
               ;;   (:module "tilemaker"
               ;;    :components ((:file "package")
               ;;                 (:file "conditions")
               ;;                 (:file "drawing-state")
               ;;                 (:file "draw-skill")
               ;;                 (:file "draw-skills")
               ;;                 (:module "draw-tile"
               ;;                  :components ((:file "common")
               ;;                               (:file "warrior")
               ;;                               (:file "module")))))))
               ;; (:file "user")
               ))

(asdf:defsystem #:nervous-island
  :description "ネウロ島六角ボードゲームエンジン"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :depends-on (;; NI dependencies
               #:nervous-island/common
               #:nervous-island/tiles
               #:nervous-island/armies
               #:nervous-island/junk
               ;; User package dependencies
               #:utilities.print-tree
               #:split-sequence)
  :pathname "src"
  :components ((:file "user"))
  :in-order-to ((test-op (load-op :nervous-island/test)))
  :perform
  (test-op (o c)
    (symbol-call '#:parachute '#:test '#:nervous-island/test
                 :report (find-symbol "INTERACTIVE"
                                      "PARACHUTE"))))

(asdf:defsystem #:nervous-island/test
  :description "Test suite for Nervous Island"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "AGPLv3"
  :version "0.0"
  :serial t
  :depends-on (#:alexandria
               #:parachute
               #:named-readtables
               #:nervous-island)
  :pathname "test"
  :components ((:file "package")
               (:module "tiles"
                :components ((:file "element")
                             (:file "token")
                             (:file "army")
                             (:file "skill")
                             (:file "attack")
                             (:file "effect")
                             (:file "tile")
                             ))
               ;; (:module "state"
               ;;  :components ((:file "coord")
               ;;               (:file "board")
               ;;               (:file "space")
               ;;               (:file "player")
               ;;               (:file "damage")
               ;;               (:file "phase")
               ;;               (:file "step")
               ;;               (:file "choice")
               ;;               (:file "state")))
               (:file "armies")))
