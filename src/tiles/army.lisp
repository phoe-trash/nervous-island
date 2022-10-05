;;;; src/elements/army.lisp

(uiop:define-package #:nervous-island.army
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common)
                    (#:nel #:nervous-island.element))
  (:export #:army #:name #:element-count #:hq-elements #:elements
           #:element-count-error #:ensure-army))

(in-package #:nervous-island.army)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nel:element-container)
  ((name :type symbol :requiredp t)
   (element-count :type (integer 1) :initform 35)
   (hq-elements :type (φ:list-of nel:hq-element) :initform '())
   (elements :type (φ:list-of nel:element) :initform '()))
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
  (when (and (not designatorsp)
             (not (and hq-elements-p elementsp))
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
      (check-type designator nel:element-designator)
      (let ((class (a:ensure-car designator))
            (count (if (consp designator) (second designator) 1)))
        (dotimes (i count)
          (push (make-instance class :owner army) result))))))

(defun set-elements-from-designators (army designators)
  (let ((all-elements (process-element-designators designators army)))
    (multiple-value-bind (hq-elements elements)
        (φ:split (a:rcurry #'typep 'nel:hq-element) all-elements)
      (setf (slot-value army '%hq-elements) hq-elements
            (slot-value army '%elements) elements))))

(defun ensure-element-owner (army)
  (flet ((frob (element)
           (if (eqv army (nel:owner element))
               element
               (copy element :owner army))))
    (setf (slot-value army '%hq-elements) (mapcar #'frob (hq-elements army))
          (slot-value army '%elements) (mapcar #'frob (elements army)))))

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
  (flet ((foreign-elements-p (elements)
           (find-if-not (a:curry #'eqv army) elements :key #'nel:owner)))
    (cond (designatorsp
           (set-elements-from-designators army designators))
          ((or (foreign-elements-p (hq-elements army))
               (foreign-elements-p (elements army)))
           ;; TODO document the reparenting behavior.
           (ensure-element-owner army))))
  (let ((element-count (element-count army)))
    (check-element-count army element-count)))

(defgeneric ensure-army (thing)
  (:method ((army army)) army)
  (:method ((symbol symbol)) (ensure-army (make-instance symbol)))
  (:method (thing)
    (error 'type-error :datum thing :expected-type '(or army symbol))))
