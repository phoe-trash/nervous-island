;;;; src/tokens/token.lisp

(uiop:define-package #:nervous-island.token
  (:use #:nervous-island.cl)
  (:local-nicknames (#:p #:protest/base)
                    (#:nel #:nervous-island.element)
                    (#:nsk #:nervous-island.skill))
  (:export #:token
           #:damage #:net
           #:venom #:takeover #:steel-net #:paralysis #:no-power #:ranged-net
           #:freezing
           #:roots #:hole #:toxic-bomb #:quicksands
           #:incubator-token #:accelerator #:claw #:attack-net #:acid-thrower
           #:quill #:left-quill #:right-quill
           #:tentacles #:zombie
           #:satiety #:lesser-satiety #:greater-satiety
           #:lungs #:claws #:eyes #:fangs #:muscles #:heart))

(in-package #:nervous-island.token)

(define-class token (nel:element) ()
  (:protocolp t))

(define-class skill-token (token nsk:skill-having) ()
  (:protocolp t)) ;; TODO use me someday

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (let* ((owner (nel:owner object))
           (name (if owner (nel:name owner) 'unowned)))
      (format stream "~A ~A ~A" name (type-of object) 'token))))

(defmacro define-token (name (&key owned (superclass 'token)))
  (let ((lambda-list (if owned '(owner) '()))
        (initargs (if owned '(:owner owner) '())))
    `(progn
       (define-class ,name (,superclass) ())
       (defun ,name (,@lambda-list)
         (make-instance ',name ,@initargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens - concrete classes

(define-token damage ())
(define-token net ())

(define-token venom (:owned t))
(define-token takeover (:owned t))
(define-token steel-net (:owned t))
(define-token paralysis (:owned t))
(define-token no-power (:owned t))
(define-token ranged-net (:owned t))
(define-token freezing (:owned t))

(define-token roots (:owned t))
(define-token hole (:owned t))
(define-token toxic-bomb (:owned t))
(define-token quicksands (:owned t))

(define-class incubator-token (token) ()
  (:protocolp t))
(define-token accelerator (:owned t :superclass incubator-token))
(define-token claw (:owned t :superclass incubator-token))
(define-token attack-net (:owned t :superclass incubator-token))
(define-token acid-thrower (:owned t :superclass incubator-token))

(define-class quill (token) ()
  (:protocolp t))
(define-token left-quill (:owned t :superclass quill))
(define-token right-quill (:owned t :superclass quill))

(define-token tentacles (:owned t))
(define-token zombie (:owned t))

;;; TODO associate tokens with skills maybe
(define-class satiety (token) ()
  (:protocolp t))
(define-class lesser-satiety (satiety) ()
  (:protocolp t))
(define-class greater-satiety (satiety) ()
  (:protocolp t))

(define-token lungs (:owned t :superclass lesser-satiety))
(define-token claws (:owned t :superclass lesser-satiety))

(define-token eyes (:owned t :superclass greater-satiety))
(define-token fangs (:owned t :superclass greater-satiety))
(define-token muscles (:owned t :superclass greater-satiety))
(define-token heart (:owned t :superclass greater-satiety))
