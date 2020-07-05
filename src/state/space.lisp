;;;; src/state/space.lisp

(uiop:define-package #:nervous-island.space
  (:use #:cl)
  (:shadow #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:Φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common)
                    (#:nt #:nervous-island.tile))
  (:export
   #:space #:tokens #:tile #:rotation #:foundation #:make-spaces #:edit-space))

(in-package #:nervous-island.space)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Space

(defclass space ()
  ((%tokens :reader tokens :initarg :tokens)
   (%tile :reader tile :initarg :tile)
   (%rotation :reader rotation :initarg :rotation)
   (%foundation :reader foundation :initarg :foundation))
  (:default-initargs :tokens '() :tile nil :rotation nil :foundation nil))

(defmethod shared-initialize :before ((space space) slots
                                      &key tile rotation foundation)
  (when tile
    (check-type tile (and nt:tile (not nt:foundation)))
    (unless rotation (a:required-argument :rotation))
    (check-type rotation ncom:direction))
  (when foundation
    (check-type foundation nt:foundation)))

(defun make-spaces (axials)
  (let ((spaces (make-hash-table :test #'equalp)))
    (dolist (axial axials spaces)
      (setf (gethash axial spaces) (make-instance 'space)))))

(defun edit-space (space &rest initargs)
  (apply #'φ:shallow-copy-object space initargs))
