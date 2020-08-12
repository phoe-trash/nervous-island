;;;; src/common/macros.lisp

(in-package #:nervous-island.common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class definition

(defun create-defclass-form (name superclasses slots options)
  (flet ((process-slot-definition (slot-form)
           (destructuring-bind (name &key (initform nil initformp)
                                &allow-other-keys)
               slot-form
             (let ((slot-name (a:symbolicate :% name))
                   (keyword (a:make-keyword name)))
               `(,slot-name :reader ,name :initarg ,keyword
                            ,@(when initformp `(:initform ,initform))))))
         (process-initform-initargs (slot-form)
           (destructuring-bind (name &key (initform nil initformp)
                                &allow-other-keys)
               slot-form
             (declare (ignore initform))
             (unless initformp
               (let ((keyword (a:make-keyword name))
                     (default-initargs (a:assoc-value options :default-initargs)))
                 (multiple-value-bind (foundp value)
                     (get-properties default-initargs (list keyword))
                   `(,keyword ,(cond (foundp value)
                                     (t `(a:required-argument ,keyword)))))))))
         (process-default-initargs ()
           (a:assoc-value options :default-initargs)))
    (let* ((protocolp (car (a:assoc-value options :protocolp)))
           (definition-symbol (if protocolp 'p:define-protocol-class 'defclass))
           (slot-definitions (mapcar #'process-slot-definition slots))
           (initform-initargs (a:mappend #'process-initform-initargs slots))
           (default-initargs (process-default-initargs)))
      `(,definition-symbol ,name ,superclasses ,slot-definitions
                           (:default-initargs ,@initform-initargs
                                              ,@default-initargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructor

(defun create-shared-initialize (name slots options)
  (a:with-gensyms (args)
    (flet
        ((decorate-slot (slot-form)
           (destructuring-bind (name &key type &allow-other-keys) slot-form
             (let* ((keyword (a:make-keyword name))
                    (var (a:make-gensym name))
                    (suffix (if (find #\- (string name)) "-P" "P"))
                    (predicate (a:make-gensym (format nil "~A~A" name suffix))))
               (list name var predicate type keyword))))
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
             `((apply ,function ,name ,args))))
         (decorate-extra-args ()
           (a:when-let ((extra-args (a:assoc-value options :extra-args)))
             (loop for keyword in extra-args
                   collect (list keyword (a:make-gensym keyword)))))
         (make-extra-key (decorated)
           (destructuring-bind (keyword gensym) decorated
             `((,keyword ,gensym))))
         (make-extra-ignore (decorated)
           (destructuring-bind (keyword gensym) decorated
             (declare (ignore keyword))
             gensym))
         (remove-extra-args (args)
           (a:if-let ((extra-args (a:assoc-value options :extra-args)))
             `(apply #'a:remove-from-plist ,args ',extra-args)
             args)))
      (let ((decorated-slots (mapcar #'decorate-slot slots))
            (decorated-extra-args (decorate-extra-args)))
        (a:with-gensyms (slots)
          (let ((keys (mapcar #'make-key decorated-slots))
                (ignores (a:mappend #'make-ignore decorated-slots))
                (extra-keys (mapcar #'make-extra-key decorated-extra-args))
                (extra-ignores (mapcar #'make-extra-ignore
                                       decorated-extra-args))
                (typechecks (mapcar #'make-typecheck decorated-slots))
                (before (make-before))
                (after (make-after))
                (new-args (remove-extra-args args)))
            `(defmethod shared-initialize :around
                 ((,name ,name) ,slots &rest ,args &key ,@keys ,@extra-keys)
               (declare (ignorable ,@ignores ,@extra-ignores))
               ,@typechecks
               ,@before
               (apply #'call-next-method ,name ,slots ,new-args)
               ,@after
               ,name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro interface

(defparameter *valid-options*
  '(:protocolp :before :after :extra-args :default-initargs))

(defparameter *valid-slot-options*
  '(:type :initform))

(defun verify-options (options)
  (loop for option in options
        for (keyword . nil) = option
        unless (member keyword *valid-options*)
          do (error "Unknown option ~S" option)))

(defun verify-slot-options (slot-options)
  (loop for option on slot-options by #'cddr
        for (keyword . nil) = option
        unless (member keyword *valid-slot-options*)
          do (error "Unknown slot option ~S" option)))

(defun %define-typechecked-class (name superclasses slot-definitions options)
  (verify-options options)
  (dolist (slot-definition slot-definitions)
    (verify-slot-options (cdr slot-definition)))
  `(progn
     ,(create-defclass-form name superclasses slot-definitions options)
     ,(create-shared-initialize name slot-definitions options)
     ',name))

(defmacro define-typechecked-class
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  (%define-typechecked-class name superclasses slot-definitions options))

(setf (trivial-indent:indentation 'define-typechecked-class)
      '(4 &lambda &body))
