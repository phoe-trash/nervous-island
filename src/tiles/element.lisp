;;;; src/elements/element.lisp

(uiop:define-package #:nervous-island.element
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common))
  (:export #:element-metacontainer #:name
           #:element-container #:owner
           #:element #:color #:hq-element #:element-designator))

(in-package #:nervous-island.element)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element

(defgeneric name (owner)
  (:method ((owner null)) 'unowned))

(deftype value () '(real 0 1))
(deftype color () '(cons value (cons value (cons value (cons value null)))))

(defgeneric color (owner)
  (:method ((color null)) '(0.5 0.5 0.5 1)))

(define-class element-metacontainer ()
  ((name :type string))
  (:protocolp t))

(deftype element-container-owner () '(or null element-metacontainer))

(define-class element-container ()
  ((owner :type element-container-owner :initform nil)
   (name :type symbol)
   (color :type color :initform '(0.5 0.5 0.5 1)))
  (:protocolp t))

(deftype element-owner () '(or null element-container))

(defmethod generic-eqv ((x element-container) (y null)) nil)

(defmethod generic-eqv ((x null) (y element-container)) nil)

(define-class element ()
  ((owner :type element-owner :initform nil))
  (:protocolp t))

(defmethod print-object ((object element) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (let* ((owner (owner object))
           (name (if owner (name owner) 'unowned)))
      (format stream "~A ~A" name (type-of object)))))

(define-class hq-element (element) ()
  (:protocolp t))

(defun element-designator-p (thing)
  (flet ((test (thing) (a:when-let ((class (find-class thing nil)))
                         (subclassp class (find-class 'element)))))
    (typecase thing
      (symbol (test thing))
      ((cons symbol) (test (car thing))))))

(deftype element-designator ()
  `(and (or symbol (cons symbol (cons (integer 1) null)))
        (satisfies element-designator-p)))
