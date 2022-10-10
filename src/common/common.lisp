;;;; src/common/common.lisp

(uiop:define-package #:nervous-island.common
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:φ #:phoe-toolbox)
                    (#:p #:protest/base)
                    (#:v #:value-semantics-utils))
  (:shadowing-import-from #:value-semantics-utils
                          #:eqv #:generic-eqv #:copy
                          #:set #:set-test #:set-contents #:set-count
                          #:set-insert #:set-remove #:set-find
                          #:set-difference #:set-union #:set-intersection
                          #:set-exclusive-or)
  (:export
   ;; Types and constants
   #:direction #:diagonal #:anywhere #:self
   #:*directions* #:*diagonals* #:*anywhere* #:*self*
   ;; Conditions
   #:nervous-island-condition #:nervous-island-error
   ;; Macros
   #:define-class
   ;; EQV and SHALLOW-COPY
   ;; TODO try to not use EQ/EQL/EQUAL/EQUALP anywhere in the codebase
   ;; TODO get rid of Φ:LIST-OF everywhere we actually mean a set
   #:eqv #:generic-eqv #:copy
   ;; Set
   #:set #:set-test #:set-contents #:set-count
   #:set-insert #:set-remove #:set-find
   #:set-difference #:set-union #:set-intersection #:set-exclusive-or))

(macrolet
    ((create-ncl-package ()
       (flet ((c2cl-symbols ()
                (let ((symbols (loop for symbol being each symbol of :c2cl
                                     collect symbol))
                      (banned-symbols
                        '(;; We use EQV as an equality predicate
                          ;; and explicitly qualify
                          ;; the CL:EQL specializer.
                          cl:eq cl:eql cl:equal cl:equalp
                          ;; SET is going to mean a data structure
                          ;; rather than an assignment operator.
                          cl:set cl:set-difference cl:set-exclusive-or
                          ;; DEFINE-CLASS is used instead of
                          ;; DEFCLASS.
                          cl:defclass)))
                  (sort (cl:set-difference symbols banned-symbols) #'string<)))
              (ncom-symbols ()
                '(#:eqv #:generic-eqv #:copy
                  #:define-class
                  #:set #:set-test #:set-contents #:set-count
                  #:set-insert #:set-remove #:set-find
                  #:set-difference #:set-union #:set-intersection
                  #:set-exclusive-or)))
         `(uiop:define-package #:nervous-island.cl
            (:use)
            (:import-from #:c2cl ,@(c2cl-symbols))
            (:import-from #:nervous-island.common ,@(ncom-symbols))
            (:export ,@(c2cl-symbols))
            (:export ,@(ncom-symbols))))))
  (create-ncl-package))

(in-package #:nervous-island.common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types and constants

(deftype direction () '(member :q :w :e :d :s :a))

(deftype diagonal () '(member :aq :qw :we :ed :ds :sa))

(deftype anywhere () '(member :anywhere))

(deftype self () '(member :self))

(defparameter *directions* '(:q :w :e :d :s :a))

(defparameter *diagonals* '(:aq :qw :we :ed :ds :sa))

(defparameter *anywhere* '(:anywhere))

(defparameter *self* '(:self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions

(define-condition nervous-island-condition () ())

(define-condition nervous-island-error (nervous-island-condition error) ())

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

(setf (trivial-indent:indentation 'define-class)
      '(4 &lambda &body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class definition

(defun create-defclass-form (name superclasses slots options)
  (flet ((process-slot-definition (slot-form)
           (destructuring-bind
               (name &key (initform nil initformp) (type t typep)
                &allow-other-keys)
               slot-form
             (let ((slot-name (a:symbolicate :% name))
                   (keyword (a:make-keyword name)))
               `(,slot-name :reader ,name :initarg ,keyword
                            ,@(when typep `(:type ,type))
                            ,@(when initformp `(:initform ,initform))))))
         (process-initform-initargs (slot-form)
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
         (process-default-initargs (initform-initargs)
           (let ((default-initargs (a:assoc-value options :default-initargs)))
             (loop for (key value) on default-initargs by #'cddr
                   unless (get-properties initform-initargs (list key))
                     collect key and collect value))))
    (let* ((protocolp (car (a:assoc-value options :protocolp)))
           (definition-symbol (if protocolp 'p:define-protocol-class 'defclass))
           (slot-definitions (mapcar #'process-slot-definition slots))
           (initform-initargs (a:mappend #'process-initform-initargs slots))
           (default-initargs (process-default-initargs initform-initargs)))
      `(,definition-symbol
        ,name ,superclasses ,slot-definitions
        (:default-initargs ,@initform-initargs
                           ,@default-initargs)
        (:metaclass v:class-with-value-semantics)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructor

(defun create-shared-initialize (name slots options)
  (a:with-gensyms (args)
    (flet
        ((decorate-slot (slot-form)
           (destructuring-bind (name &key transform &allow-other-keys)
               slot-form
             (let* ((keyword (a:make-keyword name))
                    (var (a:make-gensym name))
                    (suffix (if (find #\- (string name)) "-P" "P"))
                    (predicate (a:make-gensym (format nil "~A~A" name suffix))))
               (list name var predicate keyword transform))))
         (make-key (decorated)
           (destructuring-bind (name var predicate keyword transform)
               decorated
             (declare (ignore name transform))
             `((,keyword ,var) nil ,predicate)))
         (make-ignore (decorated)
           `(,(second decorated) ,(third decorated)))
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
                (before (make-before))
                (after (make-after))
                (new-args (remove-extra-args args)))
            `(defmethod shared-initialize :around
                 ((,name ,name) ,slots &rest ,args &key ,@keys ,@extra-keys)
               (declare (ignorable ,@ignores ,@extra-ignores))
               ,@before
               (apply #'call-next-method ,name ,slots ,new-args)
               ,@after
               ,name)))))))
