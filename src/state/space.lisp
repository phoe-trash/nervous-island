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
  (:export
   #:space #:axial #:tokens #:tile #:rotation #:foundation #:make-spaces
   #:edit-space #:edit-spaces #:find-tile #:cannot-edit-axial))

(in-package #:nervous-island.space)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Space

(define-class space ()
  ((axial :type nc:axial)
   (tokens :type (φ:list-of nto:token) :initform '())
   (unit :type (or null nt:unit) :initform nil)
   (unit-rotation :type (or null (mod 6)) :initform nil
                  :transform (lambda (x) (when x (mod x 6))))
   (foundation :type (or null nt:foundation) :initform nil))
  (:before #'make-space-before))

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
    (dolist (thing things (apply #'vs:dict (nreverse result)))
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

;; (defgeneric edit-space (space &rest initargs)
;;   (:method ((space space) &rest initargs)
;;     (apply #'φ:copy-object space initargs)))

;; (define-condition cannot-edit-axial (ncom:nervous-island-error)
;;   ((%space :reader cannot-edit-axial-space :initarg :space)
;;    (%spaces :reader cannot-edit-axial-spaces :initarg :spaces))
;;   (:default-initargs :space (a:required-argument :space)
;;                      :spaces (a:required-argument :spaces))
;;   (:report
;;    (lambda (condition stream)
;;      (format stream "Attempted to edit the axial of space ~S in spaces ~S."
;;              (cannot-edit-axial-space condition)
;;              (cannot-edit-axial-spaces condition)))))

;; (defun edit-spaces (spaces space &rest initargs &key axial &allow-other-keys)
;;   (when axial (error 'cannot-edit-axial :space space :spaces spaces))
;;   (let ((axial (axial space))
;;         (new-space (apply #'edit-space space initargs))
;;         (new-spaces (a:copy-hash-table spaces)))
;;     (setf (gethash axial new-spaces) new-space)
;;     new-spaces))

;; (defgeneric find-tile (spaces tile)
;;   (:method ((spaces hash-table) (tile nt:tile))
;;     (φ:fbind ((fn (etypecase tile
;;                     (nt:foundation #'foundation)
;;                     (nt:tile #'tile))))
;;       (maphash (lambda (axial space)
;;                  (declare (ignore axial))
;;                  (when (eq tile (funcall #'fn space))
;;                    (return-from find-tile space)))
;;                spaces))))
