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
           #:total-element-count
           #:hq-element-count #:hq-elements
           #:element-count #:elements
           #:token-count #:tokens
           #:element-count-error
           #:ensure-army #:make-all-armies))

(in-package #:nervous-island.army)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nel:element-container)
  ((total-element-count :type (integer 1) :initform 35)
   (hq-element-count :type (integer 1) :initform 1)
   (hq-elements :type (φ:list-of nel:hq-element) :initform '())
   (element-count :type (integer 1) :initform 34)
   (elements :type (φ:list-of nel:element) :initform '())
   (token-count :type (integer 0) :initform 0)
   (tokens :type (φ:list-of nto:token) :initform '()))
  (:protocolp t)
  (:extra-args :designators :token-designators :discard)
  (:before #'make-army-before)
  (:after #'make-army-after))

(defmethod print-object ((object army) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A ~A (~D elements~:[, ~D tokens~;~])"
            (nel:name object) (type-of object) (total-element-count object)
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
   (%type :reader element-count-error-type :initarg :type)
   (%army :reader element-count-error-army :initarg :army))
  (:default-initargs :actual (a:required-argument :actual)
                     :expected (a:required-argument :expected)
                     :type (a:required-argument :type)
                     :army (a:required-argument :army))
  (:report (lambda (condition stream)
             (format stream
                     "~A count error in ~S: expected ~D, but got ~D."
                     (element-count-error-type condition)
                     (element-count-error-army condition)
                     (element-count-error-expected condition)
                     (element-count-error-actual condition)))))

(defun discard-elements (army discards)
  (flet ((discard-tile (type slot)
           (let* ((predicate (lambda (x) (typep x type)))
                  (value (slot-value army slot))
                  (foundp (find-if predicate value)))
             (unless foundp
               (error "~S contains no ~S in its ~S." army type slot))
             (let ((new-value (remove-if predicate value :count 1)))
               (setf (slot-value army slot) new-value)))))
    (dolist (discard discards)
      (check-type discard symbol)
      (let ((slot (a:switch (discard :test #'subtypep)
                    ('nel:hq-element '%hq-elements)
                    ('nel:element '%elements)
                    ('nto:token '%tokens))))
        (discard-tile discard slot)))))

(defun check-element-count (army)
  (flet ((test (type expected actual)
           (unless (= expected actual)
             (error 'element-count-error :army army :type type
                                         :expected expected :actual actual))))
    (test 'nel:hq-element
           (hq-element-count army)
           (length (hq-elements army)))
    (test 'nel:element
           (element-count army)
           (length (elements army)))
    (test 'total-element-count
           (total-element-count army)
           (+ (length (hq-elements army)) (length (elements army))))
    (test 'nto:token
           (token-count army)
           (length (tokens army)))))

(defun make-army-after (army &key
                               (designators nil designatorsp)
                               (token-designators nil token-designators-p)
                               (discard '() discardp)
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
  (when discardp
    (discard-elements army discard))
  (check-element-count army))

(defgeneric ensure-army (thing)
  (:method ((army army)) army)
  (:method ((symbol symbol)) (ensure-army (make-instance symbol)))
  (:method (thing)
    (error 'type-error :datum thing :expected-type '(or army symbol))))

(defun make-all-armies ()
  (loop with prefix = (symbol-name '#:nervous-island.armies.)
        for package in (sort (copy-list (list-all-packages)) #'string<
                             :key #'package-name)
        for package-name = (package-name package)
        for result = (search prefix package-name)
        when (and result (= 0 result))
          collect (make-instance (find-symbol (symbol-name '#:army) package))))
