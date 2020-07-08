;;;; src/common.lisp

(uiop:define-package #:nervous-island.common
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base))
  (:export
   ;; Types and constants
   #:direction #:diagonal #:*directions* #:diagonals
   ;; Conditions
   #:nervous-island-condition #:nervous-island-error
   ;; Macros
   #:define-typechecked-class))

(in-package #:nervous-island.common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types and constants

(deftype direction () '(member :q :w :e :d :s :a))

(deftype diagonal () '(member :qw :wq :we :ew :ed :de :ds :sd :as :sa :aq :qa))

(defparameter *directions* '(:q :w :e :d :s :a))

(defparameter *diagonals* '(:qw :wq :we :ew :ed :de :ds :sd :as :sa :aq :qa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions

(define-condition nervous-island-condition () ())

(define-condition nervous-island-error (nervous-island-condition error) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

;;; TODO somehow fix symbol capture in the macro in order to introduce custom
;;; functions in the verifier

(eval-when (:compile-toplevel :load-toplevel :execute)
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

  (defun %define-typechecked-class
      (name superclasses slot-definitions options)
    (let* ((slot-definitions (mapcar #'a:ensure-list slot-definitions))
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
           (apply #'call-next-method ,name ,slots ,args))
         ',name))))

(defmacro define-typechecked-class
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  (%define-typechecked-class name superclasses slot-definitions options))

(setf (trivial-indent:indentation 'define-typechecked-class)
      '(4 &lambda &body))
