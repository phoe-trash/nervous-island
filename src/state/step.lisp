;;;; src/state/step.lisp

(uiop:define-package #:nervous-island.step
  (:use #:cl)
  (:shadow #:step #:number #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:Φ #:phoe-toolbox)
                    (#:np #:nervous-island.player)
                    (#:nsp #:nervous-island.space)
                    (#:nt #:nervous-island.tile))
  (:export #:define-step))

(in-package #:nervous-island.step)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defun transform-slot-definition (name &key type requiredp)
  (declare (ignore type requiredp))
  (let ((slot-name (a:symbolicate :% name))
        (keyword (a:make-keyword name)))
    `(,slot-name :reader ,name :initarg ,keyword)))

(defun transform-default-initargs (name &key type (requiredp t))
  (declare (ignore type))
  (when requiredp
    (let ((keyword (a:make-keyword name)))
      `(,keyword (a:required-argument ,keyword)))))

(defun transform-key-args (name &key type requiredp)
  (declare (ignore type requiredp))
  (let ((predicate-name (if (find #\- (symbol-name name))
                            (a:symbolicate name :-p)
                            (a:symbolicate name :p))))
    `(,name nil ,predicate-name)))

(defun transform-ignorables (name &key type requiredp)
  (declare (ignore type requiredp))
  (let ((predicate-name (if (find #\- (symbol-name name))
                            (a:symbolicate name :-p)
                            (a:symbolicate name :p))))
    `(,name ,predicate-name)))

(defun transform-typechecks (args name &key (type nil typep) requiredp)
  (declare (ignore requiredp))
  (when typep
    (let ((keyword (a:make-keyword name))
          (predicate-name (if (find #\- (symbol-name name))
                              (a:symbolicate name :-p)
                              (a:symbolicate name :p))))
      `(when ,predicate-name
         (check-type ,name ,type)
         (nconc (list ,keyword ,name) ,args)))))

;;; TODO somehow fix symbol capture in the macro in order to introduce custom
;;; functions in the verifier
(defmacro define-step
    (name (&rest superclasses) &body slot-definitions-and-options)
  (let* ((slot-definitions (car slot-definitions-and-options))
         (slot-definitions (mapcar #'a:ensure-list slot-definitions))
         (options (cdr slot-definitions-and-options))
         (superclasses (or superclasses '(step)))
         (slots (gensym "SLOTS"))
         (args (gensym "ARGS"))
         (protocolp (getf options :protocolp)))
    `(progn
       (,(if protocolp 'p:define-protocol-class 'defclass)
        ,name ,superclasses
        ,(mapcar (a:curry #'apply #'transform-slot-definition)
                 slot-definitions)
        (:default-initargs
         ,@(a:mappend (a:curry #'apply #'transform-default-initargs)
                      slot-definitions)))
       (defmethod shared-initialize :around
           ((,name ,name) ,slots &rest ,args
            &key ,@(mapcar (a:curry #'apply #'transform-key-args)
                           slot-definitions))
         (declare (ignorable
                   ,@(a:mappend (a:curry #'apply #'transform-ignorables)
                                slot-definitions)))
         ,@(mapcar (a:curry #'apply #'transform-typechecks args)
                   slot-definitions)
         ,@()
         (apply #'call-next-method ,name ,slots ,args))
       ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Steps

;;; TODO: what's the difference between steps and choices? Do we want both?

(p:define-protocol-class step () ())

(define-step player-step ()
  ((player :type np:player))
  :protocolp t)

(define-step choose-player-order (player-step)
  ((players :type (Φ:list-of np:player))))

(define-step place-tile (player-step)
  ((tile :type nt:tile)
   (space :type nsp:space)))

(define-step draw-tile (player-step)
  ((tile :type nt:tile)))

(define-step discard-tile (player-step)
  ((tile :type nt:tile)))

(define-step use-instant-tile (player-step)
  ((tile :type nt:tile)
   target))

(define-step end-turn (player-step))
