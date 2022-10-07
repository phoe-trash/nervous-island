;;;; src/tiles/skill.lisp

(uiop:define-package #:nervous-island.skill
  (:use #:nervous-island.cl)
  (:shadow #:open #:return)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common)
                    (#:nel #:nervous-island.element))
  (:export
   ;; Skills - protocol
   #:skill-having #:skills
   #:skill #:skill-printables #:directed #:direction #:undirected
   #:*activation-times* #:activation-time #:active #:activation-time #:passive
   #:zombie
   ;; Skills - concrete classes
   #:*special-initiative-values* #:initiative-value
   #:armor #:net #:redirection-input #:redirection-output #:reflection
   #:tentacles
   #:toughness #:initiative #:value #:zombie-initiative
   #:venom #:sharpshooter #:spy #:return #:open #:paralysis #:mortar
   #:underground #:powered #:revival #:charge #:devouring #:thrower
   #:bloodlust
   #:mobility #:double-mobility #:rotation #:push-back #:grab #:net-of-steel
   #:execution #:adaptation #:cannibalism
   #:sandstorm-move
   #:paralysis #:mortar #:underground #:net-of-steel
   #:underground-castling
   #:explosion
   #:chain #:explosives #:ranged-net #:sacrifice))

(in-package #:nervous-island.skill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skills - protocol

(define-class skill-having ()
  ((skills :type set :initform (set)))
  (:protocolp t))

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
  ((direction :type (or ncom:direction ncom:diagonal ncom:anywhere)))
  (:protocolp t))
(defmethod skill-printables append ((skill directed))
  (list (direction skill)))

(define-class undirected (skill) ()
  (:protocolp t))

(defvar *activation-times* '(:initiative :turn :before-battle))
(deftype activation-time ()
  '(member :initiative :turn :before-battle))

(define-class active (skill)
  ((activation-time :type activation-time :initform :initiative))
  (:protocolp t))
(defmethod skill-printables append ((skill active))
  (list (activation-time skill)))

(define-class passive (skill) ()
  (:protocolp t))

(define-class zombie (skill) ()
  (:protocolp t))

(define-class hidden (skill) ()
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
(define-skill reflection (passive directed) ())
(define-skill tentacles (passive directed) ())

(define-skill toughness (passive undirected)
  ((value :type (integer 1) :initform 1)))
(define-skill initiative (passive undirected)
  ((value :type initiative-value)))
(define-skill zombie-initiative (zombie initiative)
  ;; TODO fix this to avoid duplication maybe
  ((value :type initiative-value)))
(define-skill venom (passive undirected) ())
(define-skill sharpshooter (passive undirected) ())
(define-skill spy (passive undirected) ())
(define-skill return (passive undirected) ())
(define-skill paralysis (passive undirected) ())
(define-skill mortar (passive undirected) ())
(define-skill underground (passive undirected) ())
(define-skill powered (passive undirected) ())
(define-skill revival (passive undirected) ())
(define-skill charge (passive undirected) ())
(define-skill devouring (passive undirected) ())
(define-skill thrower (passive undirected) ())
(define-skill bloodlust (passive undirected) ())

(define-skill mobility (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill double-mobility (mobility) ()
  (:default-initargs :activation-time :turn))
(define-skill rotation (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill push-back (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill grab (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill net-of-steel (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill open (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill underground-castling (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill execution (active undirected) ()
  (:default-initargs :activation-time :turn))
(define-skill adaptation (active undirected)
  ((adapt-into :type symbol))
  (:default-initargs :activation-time :turn))
(define-skill cannibalism (active undirected) ()
  (:default-initargs :activation-time :turn))

(define-skill sandstorm-move (active undirected) ()
  (:default-initargs :activation-time :before-battle))

(define-skill explosion (active undirected) ()
  (:default-initargs :activation-time :initiative))

(define-skill chain (active undirected hidden) ()
  (:default-initargs :activation-time :turn))
(define-skill explosives (active undirected hidden) ()
  (:default-initargs :activation-time :turn))
(define-skill ranged-net (active directed hidden) ()
  (:default-initargs :activation-time :turn))
(define-skill sacrifice (active undirected hidden) ()
  (:default-initargs :activation-time :turn))

(defmethod skill-printables append ((skill toughness))
  (list (value skill)))

(defmethod skill-printables append ((skill initiative))
  (list (value skill)))
