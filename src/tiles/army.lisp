;;;; src/elements/army.lisp

(uiop:define-package #:nervous-island.army
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common))
  (:export #:element-container
           #:element #:owner #:hq-element #:copy-element #:element-designator
           #:army #:name #:element-count #:hq-elements #:elements
           #:element-count-error #:ensure-army))

(in-package #:nervous-island.army)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element

(ncom:define-typechecked-class element-container () ()
  (:protocolp t))

(ncom:define-typechecked-class element ()
  ((owner :type (or null element-container) :initform nil))
  (:protocolp t))

(p:define-protocol-class hq-element (element) ())

(defgeneric copy-element (element &rest initargs)
  (:method ((element element) &rest initargs)
    (apply #'φ:shallow-copy-object element initargs)))

(defun element-designator-p (thing)
  (flet ((test (thing) (subtypep thing 'element)))
    (typecase thing
      (symbol (test thing))
      ((cons symbol) (test (car thing))))))

(deftype element-designator ()
  `(and (or symbol (cons symbol (cons (integer 1) null)))
        (satisfies element-designator-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(ncom:define-typechecked-class army (element-container)
  ((name :type symbol)
   (element-count :type (integer 1) :initform 35)
   (hq-elements :type (φ:list-of hq-element) :initform '())
   (elements :type (φ:list-of element) :initform '()))
  (:protocolp t)
  (:extra-args :designators)
  (:before #'make-army-before)
  (:after #'make-army-after))

(defmethod print-object ((object army) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "~A ~A (~D elements)"
            (name object) (type-of object) (element-count object))))

(defun make-army-before (army &key
                                (hq-elements nil hq-elements-p)
                                (elements nil elementsp)
                                (designators nil designatorsp)
                         &allow-other-keys)
  (declare (ignore hq-elements elements designators))
  (when (and (not (or hq-elements-p elementsp designatorsp))
             (not (slot-boundp army '%hq-elements))
             (not (slot-boundp army '%elements)))
    (error 'a:simple-program-error
           :format-control
           "Must provide either DESIGNATORS or ELEMENTS and HQ-ELEMENTS."))
  (when (or hq-elements-p elementsp designatorsp)
    (flet ((die (x y)
             (error 'a:simple-program-error
                    :format-control "~S and ~S must not be provided together."
                    :format-arguments (list x y))))
      (when (and elementsp designatorsp)
        (die :elements :designators))
      (when (and hq-elements-p designatorsp)
        (die :hq-elements :designators)))))

(defun process-element-designators (designators army)
  (let ((result '()))
    (dolist (designator designators result)
      (check-type designator element-designator)
      (let ((class (a:ensure-car designator))
            (count (if (consp designator) (second designator) 1)))
        (dotimes (i count)
          (push (make-instance class :owner army) result))))))

(defun set-elements-from-designators (army designators)
  (let ((all-elements (process-element-designators designators army)))
    (multiple-value-bind (hq-elements elements)
        (φ:split (a:rcurry #'typep 'hq-element) all-elements)
      (setf (slot-value army '%hq-elements) hq-elements
            (slot-value army '%elements) elements))))

(defun ensure-element-owner (army)
  (flet ((process (elements)
           (loop for element in elements
                 collect (copy-element element :owner army))))
    (setf (slot-value army '%hq-elements) (process (hq-elements army))
          (slot-value army '%elements) (process (elements army)))))

(define-condition element-count-error (ncom:nervous-island-error)
  ((%expected :reader element-count-error-expected :initarg :expected)
   (%actual :reader element-count-error-actual :initarg :actual))
  (:default-initargs :actual (a:required-argument :actual)
                     :expected (a:required-argument :expected))
  (:report (lambda (condition stream)
             (format stream
                     "Element count error in army: expected ~D, but got ~D."
                     (element-count-error-expected condition)
                     (element-count-error-actual condition)))))

(defun check-element-count (army element-count)
  (let* ((hq-elements (hq-elements army))
         (elements (elements army))
         (expected element-count)
         (actual (+ (length hq-elements) (length elements))))
    (unless (= expected actual)
      (error 'element-count-error :expected expected :actual actual))))

(defun make-army-after (army &key (designators nil designatorsp)
                        &allow-other-keys)
  (cond (designatorsp
         (set-elements-from-designators army designators))
        ((or (find-if-not (a:curry #'eq army) (hq-elements army) :key #'owner)
             (find-if-not (a:curry #'eq army) (elements army) :key #'owner))
         (ensure-element-owner army)))
  (let ((element-count (element-count army)))
    (check-element-count army element-count)))

(defgeneric ensure-army (thing)
  (:method ((army army)) army)
  (:method ((symbol symbol)) (ensure-army (make-instance symbol)))
  (:method (thing)
    (error 'simple-type-error :datum thing :expected-type '(or army symbol))))
