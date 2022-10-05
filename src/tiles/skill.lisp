;;;; src/tiles/skill.lisp

(uiop:define-package #:nervous-island.skill
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common))
  (:export
   ;; Skills - protocol
   #:skill #:skill-printables #:directed #:direction #:undirected
   #:*activation-times* #:activation-time #:active #:activation-time #:passive
   ;; Skills - concrete classes
   #:armor #:net #:mobility #:explosion #:toughness
   #:*special-initiative-values* #:initiative-value #:initiative #:value))

(in-package #:nervous-island.skill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skills - protocol

(defgeneric skill-printables (skill)
  (:method-combination append))

(define-class skill () ()
  (:protocolp t))
(defmethod skill-printables append ((skill skill))
  (list (type-of skill)))

(defmethod print-object ((object skill) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~{~A~^ ~}" (reverse (skill-printables object)))))

(define-class directed (skill)
  ((direction :type (or ncom:direction ncom:diagonal)))
  (:protocolp t))
(defmethod skill-printables append ((skill directed))
  (list (direction skill)))

(define-class undirected (skill) ()
  (:protocolp t))

(defvar *activation-times* '(:initiative :turn))
(deftype activation-time ()
  '(member :initiative :initiative-player-choice :turn))

(define-class active (skill)
  ((activation-time :type activation-time :initform :initiative))
  (:protocolp t))
(defmethod skill-printables append ((skill active))
  (list (activation-time skill)))

(define-class passive (skill) ()
  (:protocolp t))

(defmacro define-skill (name (&rest superclasses)
                        &body ((&rest slots) &body args))
  (let* ((slot-names (mapcar #'a:ensure-car slots))
         (keywords (mapcar #'a:make-keyword slot-names))
         (default-initargs (a:assoc-value args :default-initargs))
         (directedp (member 'directed superclasses))
         (activep (member 'active superclasses))
         (directionp (and directedp (not (getf default-initargs :direction))))
         (activation-time-p (and activep (not (getf default-initargs
                                                    :activation-time))))
         (lambda-list `(,@(when directionp '(direction))
                        ,@(when activation-time-p '(activation-time))
                        ,@slot-names))
         (initargs `(,@(when directionp '(:direction direction))
                     ,@(when activation-time-p
                         '(:activation-time activation-time))
                     ,@(mapcan #'list keywords slot-names))))
    `(progn
       (define-class ,name (,@superclasses) (,@slots) ,@args)
       (defun ,name (,@lambda-list)
         (make-instance ',name ,@initargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skills - concrete classes

(defvar *special-initiative-values* '(:before :after))
(deftype initiative-value () '(or (member :before :after) (integer 0)))

(define-skill armor (passive directed) ())
(define-skill net (passive directed) ())
(define-skill toughness (passive undirected) ())
(define-skill mobility (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill explosion (active undirected) ()
  (:default-initargs :activation-time :initiative))
(define-skill initiative (passive undirected)
  ((value :type initiative-value)))

(defmethod skill-printables append ((skill initiative))
  (list (value skill)))
