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

(defmethod shared-initialize :around ((space space) slots &rest args
                                      &key tile rotation foundation)
  (check-type tile (or null (and nt:tile (not nt:foundation))))
  (when (and tile (not rotation))
    (a:required-argument :rotation))
  (check-type rotation (or null ncom:direction))
  (check-type foundation (or null nt:foundation))
  (apply #'call-next-method space slots :tile tile :rotation rotation
                                        :foundation foundation args))

(defun make-spaces (axials)
  (let ((spaces (make-hash-table :test #'equalp)))
    (dolist (axial axials spaces)
      (setf (gethash axial spaces) (make-instance 'space)))))

(defun edit-space (space &rest initargs)
  (apply #'φ:shallow-copy-object space initargs))
