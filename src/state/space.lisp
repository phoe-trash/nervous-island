;;;; src/state/space.lisp

(uiop:define-package #:nervous-island.space
  (:use #:cl)
  (:shadow #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:ncom #:nervous-island.common)
                    (#:nt #:nervous-island.tile))
  (:export #:space #:tokens #:tile #:rotation #:foundation #:make-spaces))

(in-package #:nervous-island.space)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Space

(defclass space ()
  ((%tokens :reader tokens :initarg :tokens)
   (%tile :reader tile :initarg :tile)
   (%rotation :reader rotation :initarg :rotation)
   (%foundation :reader foundation :initarg :foundation))
  (:default-initargs :tokens '() :tile nil :rotation nil :foundation nil))

(defmethod initialize-instance :after
    ((space space) &key
                     (tile nil tile-p)
                     (rotation nil rotation-p))
  (when tile-p
    (check-type tile (and nt:tile (not nt:foundation)))
    (unless rotation-p (a:required-argument :rotation))
    (check-type rotation ncom:direction)))

(defun make-spaces (axials)
  (let ((spaces (make-hash-table :test #'equalp)))
    (dolist (axial axials spaces)
      (setf (gethash axial spaces) (make-instance 'space)))))
