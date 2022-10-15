;;;; src/state/space.lisp

(uiop:define-package #:nervous-island.space
  (:use #:nervous-island.cl)
  (:shadow #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:Φ #:phoe-toolbox)
                    (#:nc #:nervous-island.coord)
                    (#:ncom #:nervous-island.common)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:export #:space
           #:axial #:tokens #:overlay #:unit #:unit-rotation #:foundation
           ;; TODO remove MAKE- prefix from constructors
           #:space #:spaces #:all-elements #:find-element #:augment-spaces))

(in-package #:nervous-island.space)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Space

(define-class space ()
  ((axial :type nc:axial)
   (tokens :type (φ:list-of nto:token) :initform '())
   (overlay :type (or null nt:instant) :initform nil)
   (unit :type (or null nt:unit) :initform nil)
   (unit-rotation :type (or null (mod 6)) :initform nil
                  :transform (lambda (x) (when x (mod x 6))))
   (foundation :type (or null nt:foundation) :initform nil))
  (:before #'make-space-before))

(defmethod print-object ((object space) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (let ((axial (axial object)))
      (format stream "~D ~D ~D~A~A~A" (nc:q axial) (nc:r axial)
              (if (tokens object) (length (tokens object)) "-")
              (if (overlay object) "O" "-")
              (if (unit object) "U" "-")
              (if (foundation object) "F" "-")))))

(defun make-space-before (space &key
                                  (unit nil unitp)
                                  (unit-rotation nil unit-rotation-p)
                          &allow-other-keys)
  (let ((unit-present-p
          (or (and (slot-boundp space '%unit) (unit space))
              (and unitp unit)))
        (unit-rotation-present-p
          (or (and (slot-boundp space '%unit-rotation) (unit-rotation space))
              (and unit-rotation-p unit-rotation))))
    (when (a:xor unit-present-p unit-rotation-present-p)
      (error 'a:simple-program-error :format-control
             "~S and ~S must be provided together."
             :format-arguments '(unit unit-rotation)))))

(deftype axial-designator () '(cons integer (cons integer null)))

(defun add-elements (space elements)
  ;; Not exported: we assume here the space is freshly constructed.
  (dolist (element elements space)
    (etypecase element
      (nto:token (push element (slot-value space '%tokens)))
      (nt:instant (setf (slot-value space 'overlay) element))
      ((cons symbol (cons (mod 6) null))
       (setf (slot-value space '%unit) (make-instance (first element))
             (slot-value space '%unit-rotation) (second element)))
      ((cons nt:unit (cons (mod 6) null))
       (setf (slot-value space '%unit) (first element)
             (slot-value space '%unit-rotation) (second element)))
      (nt:foundation (setf (slot-value space '%foundation) element)))))

(defun space (thing &rest elements)
  (let (axial space)
    (etypecase thing
      (axial-designator (setf axial (apply #'nc:axial thing)
                              space (make-instance 'space :axial axial)))
      (nc:axial (setf axial thing
                      space (make-instance 'space :axial axial)))
      (space (setf axial (axial thing)
                   space (copy thing))))
    (values (add-elements space elements) axial)))

(defun spaces (&rest things)
  (let ((result '()))
    (dolist (thing things (apply #'dict (nreverse result)))
      (multiple-value-bind (space axial) (space thing)
        (push axial result)
        (push space result)))))

(defun all-elements (space)
  (append (tokens space)
          (a:ensure-list (overlay space))
          (a:ensure-list (unit space))
          (a:ensure-list (foundation space))))

(defun find-element (dict element)
  (φ:fbind ((fn (etypecase element
                  (nt:instant #'overlay)
                  (nt:foundation #'foundation)
                  (nt:unit #'unit)
                  (nto:token (lambda (space) (find element (tokens space)))))))
    (dict-map (lambda (axial space)
                (declare (ignore axial))
                (a:when-let ((actual (funcall #'fn space)))
                  (when (eqv element actual)
                    (return-from find-element space))))
              dict)))

(defun augment-spaces (dict &rest dicts-and-spaces)
  (flet ((frob (thing)
           (etypecase thing
             (nc:axial (list thing (space thing)))
             (space (list (axial thing) thing))
             (dict (loop for (axial . space) in (dict-contents thing)
                         do (check-type axial nc:axial)
                            (check-type space space)
                         collect axial collect space)))))
    (let ((args (mapcan #'frob dicts-and-spaces)))
      (dict-union* (apply #'dict args) dict))))
