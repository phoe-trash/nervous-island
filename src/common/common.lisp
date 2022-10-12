;;;; src/common/common.lisp

(uiop:define-package #:nervous-island.common
  (:use #:c2cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:φ #:phoe-toolbox)
                    (#:p #:protest/base)
                    (#:t #:trivial-indent)
                    (#:v #:value-semantics-utils))
  (:shadowing-import-from
   #:value-semantics-utils
   #:eqv #:generic-eqv #:copy
   ;; Set
   #:set #:set-test #:set-contents #:set-count
   #:set-insert #:set-remove #:set-find #:set-map
   #:set-union #:set-intersection #:set-difference
   #:set-exclusive-or #:set-union* #:set-intersection*
   ;; Dict
   #:dict #:dict-test #:dict-contents #:dict-count
   #:dict-insert #:dict-remove #:dict-find #:dict-map
   #:dict-union #:dict-intersection #:dict-difference
   #:dict-exclusive-or #:dict-union* #:dict-intersection*)
  (:export
   ;; Types and constants
   #:direction #:diagonal #:anywhere #:self
   #:*directions* #:*diagonals* #:*anywhere* #:*self*
   ;; Conditions
   #:nervous-island-condition #:nervous-island-error
   ;; Macros
   #:define-class
   ;; EQV and COPY
   ;; TODO try to not use EQ/EQL/EQUAL/EQUALP anywhere in the codebase
   ;; TODO get rid of Φ:LIST-OF everywhere we actually mean a set
   #:eqv #:generic-eqv #:copy
   ;; Set
   #:set #:set-test #:set-contents #:set-count
   #:set-insert #:set-remove #:set-find #:set-map
   #:set-union #:set-intersection #:set-difference
   #:set-exclusive-or #:set-union* #:set-intersection*
   ;; Dict
   #:dict #:dict-test #:dict-contents #:dict-count
   #:dict-insert #:dict-remove #:dict-find #:dict-map
   #:dict-union #:dict-intersection #:dict-difference
   #:dict-exclusive-or #:dict-union* #:dict-intersection*))

(in-package #:nervous-island.common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nervous Island CL package

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
                '(;; Macros
                  #:define-class
                  ;; EQV and COPY
                  #:eqv #:generic-eqv #:copy
                  ;; Set
                  #:set #:set-test #:set-contents #:set-count
                  #:set-insert #:set-remove #:set-find #:set-map
                  #:set-union #:set-intersection #:set-difference
                  #:set-exclusive-or #:set-union* #:set-intersection*
                  ;; Dict
                  #:dict #:dict-test #:dict-contents #:dict-count
                  #:dict-insert #:dict-remove #:dict-find #:dict-map
                  #:dict-union #:dict-intersection #:dict-difference
                  #:dict-exclusive-or #:dict-union* #:dict-intersection*)))
         `(uiop:define-package #:nervous-island.cl
            (:use)
            (:import-from #:c2cl ,@(c2cl-symbols))
            (:import-from #:nervous-island.common ,@(ncom-symbols))
            (:export ,@(c2cl-symbols))
            (:export ,@(ncom-symbols))))))
  (create-ncl-package))

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
