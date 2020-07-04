;;;; src/tiles/army.lisp

(uiop:define-package #:nervous-island.army
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base))
  (:export #:army #:hq-tiles #:name #:tiles))

(in-package #:nervous-island.army)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(protest/base:define-protocol-class army ()
  ((%hq-tiles :reader hq-tiles :initarg :hq-tiles)
   (%name :reader name :initarg :name)
   (%tiles :reader tiles))
  (:default-initargs :hq-tiles (a:required-argument :hq-tiles)
                     :name (a:required-argument :name)
                     :tiles (a:required-argument :tiles)))

(defmethod print-object ((object army) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "~A ~A" (name object) 'army)))

(defmethod initialize-instance :after ((army army) &key tiles)
  (let ((result '()))
    (dolist (tile tiles)
      (check-type tile (or symbol (cons symbol (cons (integer 1) null))))
      (let ((class (a:ensure-car tile))
            (count (if (consp tile) (second tile) 1)))
        (dotimes (i count) (push (make-instance class :owner army) result))))
    (setf (slot-value army '%tiles) (nreverse result))))
