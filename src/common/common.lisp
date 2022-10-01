;;;; src/common/common.lisp

(uiop:define-package #:nervous-island.common
  (:use #:cl)
  (:shadow #:set)
  (:local-nicknames (#:a #:alexandria)
                    (#:φ #:phoe-toolbox)
                    (#:p #:protest/base)
                    (#:v #:value-semantics-utils))
  (:export
   ;; Types and constants
   #:direction #:diagonal #:*directions* #:*diagonals*
   ;; Conditions
   #:nervous-island-condition #:nervous-island-error
   ;; Macros
   #:define-class
   ;; Set
   #:set #:set-test-function #:set-contents
   #:set= #:copy-set #:set-insert #:set-remove #:set-find
   ;; Functions
   #:eqv))

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

(defclass set ()
  ((test-function :initarg :test-function :reader set-test-function)
   (contents :reader set-contents))
  (:default-initargs :test-function #'eql :contents '()))

(defmethod shared-initialize :after ((set set) slots &key)
  (let* ((test-function (set-test-function set))
         (contents-1 (set-contents set))
         (contents-2 (remove-duplicates contents-1 :test test-function)))
    (unless (= (length contents-1) (length contents-2))
      (error "Duplicates found in set contents ~S" contents-1))))

(defgeneric set= (set-1 set-2)
  (:method ((set-1 set) (set-2 set))
    (and (eq (set-test-function set-1) (set-test-function set-2))
         (a:set-equal (set-contents set-1) (set-contents set-2)
                      :test (set-test-function set-1)))))

(defgeneric copy-set (set &rest args)
  (:method ((set set) &rest args)
    (apply #'φ:shallow-copy-object set args)))

(defgeneric set-insert (set thing)
  (:method ((set set) thing)
    (let* ((contents (set-contents set))
           (foundp (member thing contents :test (set-test-function set))))
      (if foundp set (copy-set set :contents (cons thing contents))))))

(defgeneric set-remove (set thing)
  (:method ((set set) thing)
    (let* ((contents (set-contents set))
           (foundp (member thing contents :test (set-test-function set))))
      (if (not foundp)
          set
          (copy-set set :contents (remove thing contents
                                          :test (set-test-function set)))))))

(defgeneric set-find (set thing)
  (:method ((set set) thing)
    (let* ((contents (set-contents set))
           (foundp (member thing contents :test (set-test-function set))))
      (if foundp
          (values thing t)
          (values nil nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dataclass

(p:define-protocol-class dataclass () ())

(defgeneric slot-transient-p (class slot-name)
  (:method (class slot-name) nil))

(defgeneric dataclass= (dataclass-1 dataclass-2)
  (:method ((dataclass-1 dataclass) (dataclass-2 dataclass))
    (when (eq (class-of dataclass-1) (class-of dataclass-2))
      )))

;;; TODO implement a dataclass
;;; TODO copy copying mechanism here
;;; TODO implement transient slots
