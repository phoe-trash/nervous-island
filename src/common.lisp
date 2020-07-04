;;;; src/common.lisp

(uiop:define-package #:nervous-island.common
  (:use #:cl)
  (:export
   ;; Types
   #:direction #:diagonal
   ;; Constants
   #:*directions* #:diagonals))

(in-package #:nervous-island.common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types and constants

(deftype direction () '(member :q :w :e :d :s :a))

(deftype diagonal () '(member :qw :wq :we :ew :ed :de :ds :sd :as :sa :aq :qa))

(defparameter *directions* '(:q :w :e :d :s :a))

(defparameter *diagonals* '(:qw :wq :we :ew :ed :de :ds :sd :as :sa :aq :qa))
