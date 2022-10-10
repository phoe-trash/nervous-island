;;;; src/tokens/token.lisp

(uiop:define-package #:nervous-island.token
  (:use #:nervous-island.cl)
  (:local-nicknames (#:p #:protest/base)
                    (#:nel #:nervous-island.element)
                    (#:nsk #:nervous-island.skill)
                    (#:ne #:nervous-island.effect))
  (:export #:token #:skill-token
           #:damage #:net
           #:venom #:takeover #:steel-net #:paralysis #:no-power #:ranged-net
           #:freezing
           #:roots #:hole #:toxic-bomb #:quicksands
           #:incubator-token #:accelerator #:claw #:attack-net #:acid-thrower
           #:quill #:left-quill #:right-quill
           #:tentacles #:zombie
           #:satiety #:lesser-satiety #:greater-satiety
           #:lungs #:claws #:eyes #:fangs #:muscles #:heart
           #:water-adjacency))

(in-package #:nervous-island.token)

(define-class token (nel:element) ()
  (:protocolp t))

(define-class skill-token (token nsk:skill-having) ()
  (:protocolp t))

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (let* ((owner (nel:owner object))
           (name (if owner (nel:name owner) 'unowned)))
      (format stream "~A ~A ~A" name (type-of object) 'token))))

(defmacro define-token (name &key owned (superclass 'token) skill protocolp)
  (let ((lambda-list (append (when owned '(owner))
                             (when skill '(skill))))
        (initargs (append (when owned '(:owner owner))
                          (when skill '(:skill skill))))
        (options (when protocolp `((:protocolp t)))))
    `(progn
       (define-class ,name (,superclass) () ,@options)
       ,@(unless protocolp
           `((defun ,name (,@lambda-list)
               (make-instance ',name ,@initargs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens - concrete classes

(define-token damage)
(define-token net)

(define-token venom :owned t)
(define-token takeover :owned t)
(define-token steel-net :owned t)
(define-token paralysis :owned t)
(define-token no-power :owned t)
(define-token ranged-net :owned t)
(define-token freezing :owned t)

(define-token roots :owned t)
(define-token hole :owned t)
(define-token toxic-bomb :owned t)
(define-token quicksands :owned t)

(define-token incubator-token
  :superclass skill-token
  :protocolp t)
(define-token accelerator
  :owned t
  :superclass incubator-token
  :skill (ne:undirected-speed 2))
(define-token claw
  :owned t
  :superclass incubator-token
  :skill (ne:undirected-melee-officer 2))
(define-token attack-net
  :owned t
  :superclass incubator-token
  :skill (ne:undirected-net-on-melees))
(define-token acid-thrower
  :owned t
  :superclass incubator-token
  :skill (ne:undirected-explosion))

(define-token quill :protocolp t)
(define-token left-quill :owned t :superclass quill)
(define-token right-quill :owned t :superclass quill)

(define-token tentacles :owned t)
(define-token zombie :owned t)

(define-token satiety
  :superclass skill-token
  :protocolp t)

(define-token lesser-satiety
  :superclass satiety
  :protocolp t)
(define-token lungs
  :owned t
  :superclass lesser-satiety
  :skill (ne:directed-speed :self))
(define-token claws
  :owned t
  :superclass lesser-satiety
  :skill (ne:directed-melee-officer :self))

(define-token greater-satiety
  :superclass satiety
  :protocolp t)
(define-token eyes
  :owned t
  :superclass greater-satiety
  :skill (ne:directed-speed :self 2))
(define-token fangs
  :owned t
  :superclass greater-satiety
  :skill (ne:directed-melee-officer :self 2))
(define-token muscles
  :owned t
  :superclass greater-satiety
  :skill (ne:directed-mobility :self 2))
(define-token heart
  :owned t
  :superclass greater-satiety
  :skill (ne:directed-toughness :self 2))

(define-token water-adjacency :owned t)
