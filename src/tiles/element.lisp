;;;; src/elements/element.lisp

(uiop:define-package #:nervous-island.element
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common))
  (:export #:owner #:auto-reparenting
           #:reparent-predicate #:reparent-reader #:reparent-writer
           #:color #:colored #:name
           #:element-metacontainer #:element-container #:element
           #:hq-element #:element-designator))

(in-package #:nervous-island.element)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Protocol (TODO this should go into NCOM)

(deftype color-value () '(real 0 1))

(deftype color ()
  (loop repeat 4
        with type = 'null
        do (setf type (list 'cons 'color-value type))
        finally (return type)))

(defgeneric color (owner)
  (:method ((color null)) '(0.5 0.5 0.5 1)))

(define-class colored ()
  ;; TODO nullable colors someday
  ((color :type color :initform '(0.5 0.5 0.5 1)))
  (:protocolp t))

(defgeneric name (thing)
  (:method ((thing null)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-reparenting

(defgeneric owner (thing))

(define-class owned ()
  ((owner :initform nil)))

;;; TODO test these

(defgeneric reparent-predicate (thing)
  (:method (thing) (constantly nil)))

(defgeneric reparent-reader (thing)
  (:method (thing) (constantly nil)))

(defgeneric reparent-writer (thing)
  (:method (thing) (constantly nil)))

(define-class auto-reparenting () ()
  (:protocolp t))

(defmethod shared-initialize :after
    ((instance auto-reparenting) slot-names &key)
  (when (funcall (reparent-predicate instance) instance)
    (flet ((reparent (element)
             (if (eqv instance (owner element))
                 element
                 (copy element :owner instance))))
      (let* ((value (funcall (reparent-reader instance) instance))
             (new-value (mapcar #'reparent value)))
        (funcall (reparent-writer instance) new-value instance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element metacontainer

(define-class element-metacontainer (colored auto-reparenting)
  ((name :type symbol))
  (:protocolp t))

(deftype element-container-owner () '(or null element-metacontainer))

(defmethod generic-eqv ((x element-metacontainer) (y null)) nil)

(defmethod generic-eqv ((x null) (y element-metacontainer)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element container

(define-class element-container (owned colored auto-reparenting)
  ((owner :type element-container-owner :initform nil)
   (name :type symbol))
  (:protocolp t))

(deftype element-owner () '(or null element-container))

(defmethod generic-eqv ((x element-container) (y null)) nil)

(defmethod generic-eqv ((x null) (y element-container)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element

(define-class element (owned)
  ((owner :type element-owner :initform nil))
  (:protocolp t))

(defmethod print-object ((object element) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (let* ((name (or (name (owner object)) 'unowned)))
      (format stream "~A ~A" name (type-of object)))))

(define-class hq-element (element) ()
  (:protocolp t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element designator

(defun element-designator-p (thing)
  (flet ((test (thing) (a:when-let ((class (find-class thing nil)))
                         (subclassp class (find-class 'element)))))
    (typecase thing
      (symbol (test thing))
      ((cons symbol) (test (car thing))))))

(deftype element-designator ()
  `(and (or symbol (cons symbol (cons (integer 1) null)))
        (satisfies element-designator-p)))
