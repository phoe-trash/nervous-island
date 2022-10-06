;;;; src/tiles/skill.lisp

(uiop:define-package #:nervous-island.skill
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common))
  (:export
   ;; Skills - protocol
   #:skill #:skill-printables #:directed #:direction #:undirected
   #:*activation-times* #:activation-time #:active #:activation-time #:passive
   ;; Skills - concrete classes
   #:*special-initiative-values* #:initiative-value
   #:armor #:net #:redirection-input #:redirection-output
   #:toughness #:initiative #:value
   #:venom #:sharpshooter #:spy
   #:mobility #:push-back #:grab #:explosion))

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
  (flet ((slot-initform-p (slot)
           (get-properties (cdr (a:ensure-list slot)) '(:initform)))
         (slot-initform-pair (slot)
           (let ((slot (a:ensure-list slot)))
             (list (car slot)
                   (nth-value 1 (get-properties (cdr slot) '(:initform)))))))
    (multiple-value-bind (slots-initform slots-no-initform)
        (φ:split #'slot-initform-p slots)
      (let* ((slot-names (mapcar #'a:ensure-car slots))
             (slot-names-no-initform (mapcar #'a:ensure-car slots-no-initform))
             (slot-names-initform (mapcar #'slot-initform-pair slots-initform))
             (keywords (mapcar #'a:make-keyword slot-names))
             (default-initargs (a:assoc-value args :default-initargs))
             (directedp (member 'directed superclasses))
             (activep (member 'active superclasses))
             (directionp (and directedp
                              (not (getf default-initargs :direction))))
             (activation-time-p (and activep (not (getf default-initargs
                                                        :activation-time))))
             (lambda-list `(,@(when directionp '(direction))
                            ,@(when activation-time-p '(activation-time))
                            ,@slot-names-no-initform
                            &optional
                            ,@slot-names-initform))
             (initargs `(,@(when directionp '(:direction direction))
                         ,@(when activation-time-p
                             '(:activation-time activation-time))
                         ,@(mapcan #'list keywords slot-names))))
        `(progn
           (define-class ,name (,@superclasses) (,@slots) ,@args)
           (defun ,name (,@lambda-list)
             (make-instance ',name ,@initargs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skills - concrete classes

(defvar *special-initiative-values* '(:before :after))
(deftype initiative-value () '(or (member :before :after) (integer 0)))

(define-skill armor (passive directed) ())
(define-skill net (passive directed) ())
(define-skill redirection-input (passive directed) ())
(define-skill redirection-output (passive directed) ())

(define-skill toughness (passive undirected)
  ((value :type (integer 1) :initform 1)))
(define-skill initiative (passive undirected)
  ((value :type initiative-value)))
(define-skill venom (passive undirected) ())
(define-skill sharpshooter (passive undirected) ())
(define-skill spy (passive undirected) ())

(define-skill mobility (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill push-back (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill grab (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill explosion (active undirected) ()
  (:default-initargs :activation-time :initiative))

(defmethod skill-printables append ((skill toughness))
  (list (value skill)))

(defmethod skill-printables append ((skill initiative))
  (list (value skill)))
