(in-package #:nervous-island.common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class definition

(defun class-process-slot-definition (slot-form)
  (destructuring-bind
      (name &key (initform nil initformp) (type t typep)
       &allow-other-keys)
      slot-form
    (let ((slot-name (a:symbolicate :% name))
          (keyword (a:make-keyword name)))
      `(,slot-name :reader ,name :initarg ,keyword
                   ,@(when typep `(:type ,type))
                   ,@(when initformp `(:initform ,initform))))))

(defun class-process-initform-initargs (slot-form options)
  (destructuring-bind (name &key
                              (initform nil initformp)
                              (requiredp nil requiredpp)
                       &allow-other-keys)
      slot-form
    (declare (ignore initform))
    (unless (or initformp (and requiredpp (not requiredp)))
      (let ((keyword (a:make-keyword name))
            (default-initargs (a:assoc-value options
                                             :default-initargs)))
        (multiple-value-bind (foundp value)
            (get-properties default-initargs (list keyword))
          `(,keyword ,(cond (foundp value)
                            (t `(a:required-argument ,keyword)))))))))

(defun class-process-default-initargs (initform-initargs options)
  (let ((default-initargs (a:assoc-value options :default-initargs)))
    (loop for (key value) on default-initargs by #'cddr
          unless (get-properties initform-initargs (list key))
            collect key and collect value)))

(defun create-defclass-form (name superclasses slots options)
  (let* ((protocolp (car (a:assoc-value options :protocolp)))
         (definition-symbol (if protocolp 'p:define-protocol-class 'defclass))
         (slot-definitions (mapcar #'class-process-slot-definition slots))
         (initform-initargs
           (a:mappend (a:rcurry #'class-process-initform-initargs options)
                      slots))
         (default-initargs
           (class-process-default-initargs initform-initargs options)))
    `(,definition-symbol
      ,name ,superclasses ,slot-definitions
      (:default-initargs ,@initform-initargs
                         ,@default-initargs)
      (:metaclass v:class-with-value-semantics))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shared initialization

;; Slot decoration
(defun si-decorate-slot (slot-form)
  (destructuring-bind (name &key transform &allow-other-keys)
      slot-form
    (let* ((keyword (a:make-keyword name))
           (var (a:make-gensym name))
           (suffix (if (find #\- (string name)) "-P" "P"))
           (predicate (a:make-gensym (format nil "~A~A" name suffix))))
      (list name var predicate keyword transform))))

(defun si-decorate-extra-args (options)
  (a:when-let ((extra-args (a:assoc-value options :extra-args)))
    (loop for keyword in extra-args
          collect (list keyword (a:make-gensym keyword)))))

;; Argument handling
(defun si-make-key (decorated)
  (destructuring-bind (name var predicate keyword transform)
      decorated
    (declare (ignore name transform))
    `((,keyword ,var) nil ,predicate)))

(defun si-make-transform (decorated)
  (destructuring-bind (name var predicate keyword transform)
      decorated
    (declare (ignore name keyword))
    (when transform
      `((when ,predicate (setf ,var (funcall ,transform ,var)))))))

(defun si-make-transform-arg (decorated)
  (destructuring-bind (name var predicate keyword transform)
      decorated
    (declare (ignore name))
    (when transform `((when ,predicate (list ,keyword ,var))))))

(defun si-make-ignore (decorated)
  `(,(second decorated) ,(third decorated)))

(defun si-make-extra-key (decorated)
  (destructuring-bind (keyword gensym) decorated
    `((,keyword ,gensym))))

(defun si-make-extra-ignore (decorated)
  (destructuring-bind (keyword gensym) decorated
    (declare (ignore keyword))
    gensym))

(defun si-make-before (args name options)
  (a:when-let ((function (car (a:assoc-value options :before))))
    `((apply ,function ,name ,args))))

(defun si-make-after (args name options)
  (a:when-let ((function (car (a:assoc-value options :after))))
    `((apply ,function ,name ,args))))

(defun si-remove-extra-args (args options)
  (a:if-let ((extra-args (a:assoc-value options :extra-args)))
    `(apply #'a:remove-from-plist ,args ',extra-args)
    args))

(defun create-shared-initialize (name slots options)
  (a:with-gensyms (args)
    (let ((decorated-slots (mapcar #'si-decorate-slot slots))
          (decorated-extras (si-decorate-extra-args options)))
      (a:with-gensyms (slots)
        (let ((keys (mapcar #'si-make-key decorated-slots))
              (transforms (a:mappend #'si-make-transform decorated-slots))
              (transform-args (a:mappend #'si-make-transform-arg
                                         decorated-slots))
              (ignores (a:mappend #'si-make-ignore decorated-slots))
              (extra-keys (mapcar #'si-make-extra-key decorated-extras))
              (extra-ignores (mapcar #'si-make-extra-ignore decorated-extras))
              (before (si-make-before args name options))
              (after (si-make-after args name options))
              (new-args (si-remove-extra-args args options)))
          `(defmethod shared-initialize :around
               ((,name ,name) ,slots &rest ,args &key ,@keys ,@extra-keys)
             (declare (ignorable ,@ignores ,@extra-ignores))
             ,@transforms
             ,@before
             (apply #'call-next-method ,name ,slots
                    (append ,@transform-args ,new-args))
             ,@after
             ,name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro interface

(defparameter *valid-options*
  '(:protocolp :before :after :extra-args :default-initargs))

(defparameter *valid-slot-options*
  '(:type :initform :requiredp :transform))

(defun verify-options (options)
  (loop for option in options
        for (keyword . nil) = option
        unless (member keyword *valid-options*)
          do (error "Unknown option ~S." option)))

(defun verify-slot-options (slot-options)
  (loop for option on slot-options by #'cddr
        for (keyword . nil) = option
        unless (member keyword *valid-slot-options*)
          do (error "Unknown slot option ~S." option)))

(defun %define-class (name superclasses slot-definitions options)
  (verify-options options)
  (dolist (slot-definition slot-definitions)
    (verify-slot-options (cdr slot-definition)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(create-defclass-form name superclasses slot-definitions options)
     ,(create-shared-initialize name slot-definitions options)
     ',name))

(defmacro define-class
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  (%define-class name superclasses slot-definitions options))

(setf (t:indentation 'define-class) '(as defclass))
