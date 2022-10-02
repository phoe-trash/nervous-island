;;;; src/common/common.lisp

(uiop:define-package #:nervous-island.common
  (:use #:cl)
  (:shadow #:set)
  (:local-nicknames (#:a #:alexandria)
                    (#:φ #:phoe-toolbox)
                    (#:p #:protest/base)
                    (#:v #:value-semantics-utils))
  (:import-from #:value-semantics-utils #:eqv #:generic-eqv #:copy)
  (:export
   ;; Types and constants
   #:direction #:diagonal #:*directions* #:*diagonals*
   ;; Conditions
   #:nervous-island-condition #:nervous-island-error
   ;; Macros
   #:define-class
   ;; EQV and SHALLOW-COPY
   ;; TODO try to not use EQ/EQL/EQUAL/EQUALP anywhere in the codebase
   #:eqv #:generic-eqv #:copy
   ;; Set
   #:set #:set-test-function #:set-contents
   #:set= #:copy-set #:set-insert #:set-remove #:set-find))

(macrolet
    ((create-ncl-package ()
       (flet ((c2cl-symbols ()
                (let ((symbols (loop for symbol being each symbol of :c2cl
                                     collect symbol))
                      (banned-symbols '(;; We use EQV as an equality predicate
                                        ;; and explicitly qualify
                                        ;; the CL:EQL specializer.
                                        cl:eq cl:eql cl:equal cl:equalp
                                        ;; SET is going to mean a data structure
                                        ;; rather than an assignment operator.
                                        cl:set
                                        ;; DEFINE-CLASS is used instead of
                                        ;; DEFCLASS.
                                        cl:defclass)))
                  (sort (set-difference symbols banned-symbols) #'string<)))
              (ncom-symbols ()
                '(#:eqv #:generic-eqv #:copy
                  ;; TODO uncomment this
                  ;; #:set #:set-test-function #:set-contents
                  ;; #:set= #:copy-set #:set-insert #:set-remove #:set-find
                  #:define-class)))
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

(defparameter *directions* '(:q :w :e :d :s :a))

(defparameter *diagonals* '(:aq :qw :we :ed :ds :sa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions

(define-condition nervous-island-condition () ())

(define-condition nervous-island-error (nervous-island-condition error) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set

;; (defclass set ()
;;   ((test-function :initarg :test-function :reader set-test-function)
;;    (contents :reader set-contents))
;;   (:default-initargs :test-function #'eql :contents '()))

;; (defmethod shared-initialize :after ((set set) slots &key)
;;   (let* ((test-function (set-test-function set))
;;          (contents-1 (set-contents set))
;;          (contents-2 (remove-duplicates contents-1 :test test-function)))
;;     (unless (= (length contents-1) (length contents-2))
;;       (setf (slot-value set 'contents) contents-2))))

;; (defgeneric set= (set-1 set-2)
;;   (:method ((set-1 set) (set-2 set))
;;     (and (eq (set-test-function set-1) (set-test-function set-2))
;;          (a:set-equal (set-contents set-1) (set-contents set-2)
;;                       :test (set-test-function set-1)))))

;; (defgeneric copy-set (set &rest args)
;;   (:method ((set set) &rest args)
;;     (apply #'φ:copy-object set args)))

;; (defgeneric set-insert (set thing)
;;   (:method ((set set) thing)
;;     (let* ((contents (set-contents set))
;;            (foundp (member thing contents :test (set-test-function set))))
;;       (if foundp set (copy-set set :contents (cons thing contents))))))

;; (defgeneric set-remove (set thing)
;;   (:method ((set set) thing)
;;     (let* ((contents (set-contents set))
;;            (foundp (member thing contents :test (set-test-function set))))
;;       (if (not foundp)
;;           set
;;           (copy-set set :contents (remove thing contents
;;                                           :test (set-test-function set)))))))

;; (defgeneric set-find (set thing)
;;   (:method ((set set) thing)
;;     (let* ((contents (set-contents set))
;;            (foundp (member thing contents :test (set-test-function set))))
;;       (if foundp
;;           (values thing t)
;;           (values nil nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dataclass

;; (p:define-protocol-class dataclass () ())

;; (defgeneric slot-transient-p (class slot-name)
;;   (:method (class slot-name) nil))

;; (defgeneric dataclass= (dataclass-1 dataclass-2)
;;   (:method ((dataclass-1 dataclass) (dataclass-2 dataclass))
;;     (when (eq (class-of dataclass-1) (class-of dataclass-2))
;;       )))

;;; TODO implement a dataclass
;;; TODO copy copying mechanism here
;;; TODO implement transient slots

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
         (process-default-initargs ()
           (a:assoc-value options :default-initargs)))
    (let* ((protocolp (car (a:assoc-value options :protocolp)))
           (definition-symbol (if protocolp 'p:define-protocol-class 'defclass))
           (slot-definitions (mapcar #'process-slot-definition slots))
           (initform-initargs (a:mappend #'process-initform-initargs slots))
           (default-initargs (process-default-initargs)))
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
