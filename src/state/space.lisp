;;;; src/state/space.lisp

(uiop:define-package #:nervous-island.space
  (:use #:nervous-island.cl)
  (:shadow #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:Φ #:phoe-toolbox)
                    (#:vs #:value-semantics-utils)
                    (#:nc #:nervous-island.coord)
                    (#:ncom #:nervous-island.common)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:export #:space
           #:axial #:tokens #:overlay #:unit #:unit-rotation #:foundation
           #:make-spaces #:augment-spaces #:find-element))

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
      (format stream "(~D ~D) ~D~A~A~A" (nc:q axial) (nc:r axial)
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

(defun make-spaces (&rest things)
  (let ((result '()))
    (dolist (thing things (apply #'dict (nreverse result)))
      (let (axial space)
        (etypecase thing
          (nc:axial (setf axial thing
                          space (make-instance 'space :axial axial)))
          (space (setf axial (axial thing)
                       space thing)))
        (push axial result)
        (push space result)
        ;; TODO this should be validated on the NI.BOARD side
        ;; (if (gethash axial spaces)
        ;;     (error 'nb:duplicated-axial :axial axial)
        ;;     (setf (gethash axial spaces) space))
        ))))

(defun augment-spaces (dict &rest dicts-and-spaces)
  (flet ((frob (thing)
           (etypecase thing
             (space (list (axial thing) thing))
             (dict (loop for (axial . thing) in (dict-contents thing)
                         collect axial collect thing)))))
    (let ((args (mapcan #'frob dicts-and-spaces)))
      (dict-union* (apply #'dict args) dict))))

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
