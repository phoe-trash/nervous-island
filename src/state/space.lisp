;;;; src/state/space.lisp

(uiop:define-package #:nervous-island.space
  (:use #:cl)
  (:shadow #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:Φ #:phoe-toolbox)
                    (#:nb #:nervous-island.board)
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

(defclass space ()
  ((%axial :reader axial :initarg :axial)
   (%tokens :reader tokens :initarg :tokens)
   (%tile :reader tile :initarg :tile)
   (%rotation :reader rotation :initarg :rotation)
   (%foundation :reader foundation :initarg :foundation))
  (:default-initargs :axial (a:required-argument :axial)
                     :tokens '() :tile nil :rotation nil :foundation nil))

(defmethod shared-initialize :around
    ((space space) slots &rest args &key
                                      (axial nil axialp)
                                      (tokens nil tokensp)
                                      (tile nil tilep)
                                      (rotation nil rotationp)
                                      (foundation nil foundationp))
  (when axialp
    (check-type axial nc:axial)
    (nconc (list :axial axial) args))
  (when tokensp
    (check-type tokens list)
    (loop for cons on tokens
          do (check-type (car cons) nto:token))
    (nconc (list :tokens tokens) args))
  (when tilep
    (check-type tile (or null (and nt:tile (not nt:foundation))))
    (nconc (list :tile tile) args))
  (when rotationp
    (check-type rotation (or null ncom:direction))
    (nconc (list :rotation rotation) args))
  (when foundationp
    (check-type foundation (or null nt:foundation))
    (nconc (list :foundation foundation) args))
  (let ((tile-present-p (or (and tilep tile)
                            (slot-boundp space '%tile)))
        (rotation-present-p (or (and rotationp rotation)
                                (slot-boundp space '%rotation))))
    (when (a:xor tile-present-p rotation-present-p)
      (cond ((not tile-present-p) (a:required-argument :tile))
            ((not rotation-present-p) (a:required-argument :rotation)))))
  (apply #'call-next-method space slots args))

(defun make-spaces (&rest things)
  (let ((spaces (make-hash-table :test #'equalp)))
    (dolist (thing things spaces)
      (let (axial space)
        (etypecase thing
          (nc:axial (setf axial thing
                          space (make-instance 'space :axial axial)))
          (space (setf axial (axial thing)
                       space thing)))
        (if (gethash axial spaces)
            (error 'nb:duplicated-axial :axial axial)
            (setf (gethash axial spaces) space))))))

(defgeneric edit-space (space &rest initargs)
  (:method ((space space) &rest initargs)
    (apply #'φ:shallow-copy-object space initargs)))

(define-condition cannot-edit-axial (ncom:nervous-island-error)
  ((%space :reader cannot-edit-axial-space :initarg :space)
   (%spaces :reader cannot-edit-axial-spaces :initarg :spaces))
  (:default-initargs :space (a:required-argument :space)
                     :spaces (a:required-argument :spaces))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to edit the axial of space ~S in spaces ~S."
             (cannot-edit-axial-space condition)
             (cannot-edit-axial-spaces condition)))))

(defun edit-spaces (spaces space &rest initargs &key axial &allow-other-keys)
  (when axial (error 'cannot-edit-axial :space space :spaces spaces))
  (let ((axial (axial space))
        (new-space (apply #'edit-space space initargs))
        (new-spaces (a:copy-hash-table spaces)))
    (setf (gethash axial new-spaces) new-space)
    new-spaces))

(defgeneric find-tile (spaces tile)
  (:method ((spaces hash-table) (tile nt:tile))
    (φ:fbind ((fn (etypecase tile
                    (nt:foundation #'foundation)
                    (nt:tile #'tile))))
      (maphash (lambda (axial space)
                 (declare (ignore axial))
                 (when (eq tile (funcall #'fn space))
                   (return-from find-tile space)))
               spaces))))
