;;;; src/elements/army.lisp

(uiop:define-package #:nervous-island.army
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common)
                    (#:nel #:nervous-island.element)
                    (#:nto #:nervous-island.token))
  (:export #:army
           #:element-count #:hq-elements #:elements #:token-count #:tokens
           #:element-count-error #:ensure-army))

(in-package #:nervous-island.army)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nel:element-container)
  ((element-count :type (integer 1) :initform 35)
   (hq-elements :type (φ:list-of nel:hq-element) :initform '())
   (elements :type (φ:list-of nel:element) :initform '())
   (token-count :type (integer 0) :initform 0)
   (tokens :type (φ:list-of nto:token) :initform '()))
  (:protocolp t)
  (:extra-args :designators :token-designators)
  (:before #'make-army-before)
  (:after #'make-army-after))

(defmethod print-object ((object army) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A ~A (~D elements~:[, ~D tokens~;~])"
            (nel:name object) (type-of object) (element-count object)
            (= 0 (token-count object)) (token-count object))))

(defun make-army-before (army &key
                                (hq-elements nil hq-elements-p)
                                (elements nil elementsp)
                                (designators nil designatorsp)
                                (tokens nil tokensp)
                                (token-designators nil token-designators-p)
                         &allow-other-keys)
  (declare (ignore hq-elements elements designators tokens token-designators))
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
        (die :hq-elements :designators))
      ;; TODO write unit tests for tokens
      (when (and tokensp token-designators-p)
        (die :tokens :token-designators)))))

(defun process-element-designators (designators army)
  (let ((result '()))
    (dolist (designator designators (nreverse result))
      (check-type designator nel:element-designator)
      (let* ((class (a:ensure-car designator))
             (count (if (consp designator) (second designator) 1))
             (instance (make-instance class :owner army)))
        (dotimes (i count)
          (push instance result))))))

(defun set-elements-from-designators
    (army &key (designators nil designatorsp)
            (token-designators nil token-designators-p))
  (when designatorsp
    (let ((all-elements (process-element-designators designators army)))
      (multiple-value-bind (hq-elements elements)
          (φ:split (a:rcurry #'typep 'nel:hq-element) all-elements)
        (setf (slot-value army '%hq-elements) hq-elements
              (slot-value army '%elements) elements))))
  (when token-designators-p
    (let ((tokens (process-element-designators token-designators army)))
      (setf (slot-value army '%tokens) tokens))))

(defun ensure-element-owner (army)
  (flet ((frob (element)
           (if (eqv army (nel:owner element))
               element
               (copy element :owner army))))
    (setf (slot-value army '%hq-elements) (mapcar #'frob (hq-elements army))
          (slot-value army '%elements) (mapcar #'frob (elements army))
          (slot-value army '%tokens) (mapcar #'frob (tokens army)))))

(define-condition element-count-error (ncom:nervous-island-error)
  ((%expected :reader element-count-error-expected :initarg :expected)
   (%actual :reader element-count-error-actual :initarg :actual)
   (%type :reader element-count-error-type :initarg :type))
  (:default-initargs :actual (a:required-argument :actual)
                     :expected (a:required-argument :expected)
                     :type (a:required-argument :type))
  (:report (lambda (condition stream)
             (format stream
                     "~:(~A~) count error in army: expected ~D, but got ~D."
                     (element-count-error-type condition)
                     (element-count-error-expected condition)
                     (element-count-error-actual condition)))))

(defun check-element-count (army element-count token-count)
  (let* ((hq-elements (hq-elements army))
         (elements (elements army))
         (tokens (tokens army))
         (actual-element-count (+ (length hq-elements) (length elements)))
         (actual-token-count (length tokens)))
    (unless (= element-count actual-element-count)
      (error 'element-count-error :type 'nel:element
                                  :expected element-count
                                  :actual actual-element-count))
    (unless (= token-count actual-token-count)
      (error 'element-count-error :type 'nto:token
                                  :expected token-count
                                  :actual actual-token-count))))

(defun make-army-after (army &key
                               (designators nil designatorsp)
                               (token-designators nil token-designators-p)
                        &allow-other-keys)
  (flet ((foreign-elements-p (elements)
           (find-if-not (a:curry #'eqv army) elements :key #'nel:owner)))
    (when designatorsp
      (set-elements-from-designators army :designators designators))
    (when token-designators-p
      (set-elements-from-designators army :token-designators token-designators))
    (when (or (foreign-elements-p (hq-elements army))
              (foreign-elements-p (elements army))
              (foreign-elements-p (tokens army)))
      ;; TODO document the reparenting behavior.
      (ensure-element-owner army)))
  (let ((element-count (element-count army))
        (token-count (token-count army)))
    (check-element-count army element-count token-count)))

(defgeneric ensure-army (thing)
  (:method ((army army)) army)
  (:method ((symbol symbol)) (ensure-army (make-instance symbol)))
  (:method (thing)
    (error 'type-error :datum thing :expected-type '(or army symbol))))
