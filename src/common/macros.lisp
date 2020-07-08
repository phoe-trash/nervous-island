;;;; src/common/macros.lisp

(in-package #:nervous-island.common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class definition

(defun create-defclass-form (name superclasses slots options)
  (flet ((process-slot-definition (slot-form)
           (destructuring-bind (name . rest) slot-form
             (declare (ignore rest))
             (let ((slot-name (a:symbolicate :% name))
                   (keyword (a:make-keyword name)))
               `(,slot-name :reader ,name :initarg ,keyword))))
         (process-default-initargs (slot-form)
           (destructuring-bind (name &key (requiredp t) &allow-other-keys)
               slot-form
             (when requiredp
               (let ((keyword (a:make-keyword name)))
                 `(,keyword (a:required-argument ,keyword)))))))
    (let* ((protocolp (car (a:assoc-value options :protocolp)))
           (definition-symbol (if protocolp 'p:define-protocol-class 'defclass))
           (slot-definitions (mapcar #'process-slot-definition slots))
           (default-initargs (a:mappend #'process-default-initargs slots)))
      `(,definition-symbol ,name ,superclasses ,slot-definitions
                           (:default-initargs ,@default-initargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructor

(defun create-shared-initialize (name slots options)
  (a:with-gensyms (args)
    (flet ((decorate (slot-form)
             (destructuring-bind (name &key type &allow-other-keys) slot-form
               (let ((keyword (a:make-keyword name)))
                 (a:with-gensyms (var predicate)
                   (list name var predicate type keyword)))))
           (make-key (decorated)
             (destructuring-bind (name var predicate type keyword) decorated
               (declare (ignore name type))
               `((,keyword ,var) nil ,predicate)))
           (make-ignore (decorated)
             `(,(second decorated) ,(third decorated)))
           (make-typecheck (decorated)
             (destructuring-bind (name var predicate type keyword) decorated
               (declare (ignore name))
               `(when ,predicate
                  (check-type ,var ,type)
                  (nconc (list ,keyword ,var) ,args))))
           (make-before ()
             (a:when-let ((function (car (a:assoc-value options :before))))
               `((apply ,function ,name ,args))))
           (make-after ()
             (a:when-let ((function (car (a:assoc-value options :after))))
               `((apply ,function ,name ,args)))))
      (let ((decorated-slots (mapcar #'decorate slots)))
        (a:with-gensyms (slots)
          (let ((keys (mapcar #'make-key decorated-slots))
                (ignores (a:mappend #'make-ignore decorated-slots))
                (typechecks (mapcar #'make-typecheck decorated-slots))
                (before (make-before))
                (after (make-after)))
            `(defmethod shared-initialize :around
                 ((,name ,name) ,slots &rest ,args &key ,@keys)
               (declare (ignorable ,@ignores))
               ,@typechecks
               ,@before
               (apply #'call-next-method ,name ,slots ,args)
               ,@after
               ,name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro interface

(defun %define-typechecked-class (name superclasses slot-definitions options)
  `(progn
     ,(create-defclass-form name superclasses slot-definitions options)
     ,(create-shared-initialize name slot-definitions options)
     ',name))

(defmacro define-typechecked-class
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  (%define-typechecked-class name superclasses slot-definitions options))

(setf (trivial-indent:indentation 'define-typechecked-class)
      '(4 &lambda &body))
