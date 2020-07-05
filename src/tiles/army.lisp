;;;; src/tiles/army.lisp

(uiop:define-package #:nervous-island.army
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common))
  (:export #:army #:name #:tile-count #:hq-tiles #:tiles #:tile-designator
           #:tile-count-error))

(in-package #:nervous-island.army)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(protest/base:define-protocol-class army ()
  ((%name :reader name :initarg :name)
   (%tile-count :reader tile-count :initarg :tile-count)
   (%hq-tiles :reader hq-tiles)
   (%tiles :reader tiles))
  (:default-initargs :name (a:required-argument :name)
                     :tile-count 35
                     :hq-tiles (a:required-argument :hq-tiles)
                     :tiles (a:required-argument :tiles)))

(defmethod print-object ((object army) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "~A ~A" (name object) 'army)))

(deftype tile-designator () '(or symbol (cons symbol (cons (integer 1) null))))

(defmethod shared-initialize :around
    ((army army) slots &rest args &key
                                    (name nil namep)
                                    (tile-count nil tile-count-p)
                                    (hq-tiles nil hq-tiles-p)
                                    (tiles nil tilesp))
  (when namep
    (check-type name symbol)
    (nconc (list :name name) args))
  (when tile-count-p
    (check-type tile-count (integer 1))
    (nconc (list :tile-count tile-count) args))
  (when hq-tiles-p
    (check-type hq-tiles list)
    (loop for cons on hq-tiles do (check-type (car cons) tile-designator))
    (nconc (list :hq-tiles hq-tiles) args))
  (when tilesp
    (check-type tiles list)
    (loop for cons on tiles do (check-type (car cons) tile-designator))
    (nconc (list :tiles tiles) args))
  (apply #'call-next-method army slots args))

(defun process-tile-designators (designators army)
  (let ((result '()))
    (dolist (designator designators result)
      (check-type designator tile-designator)
      (let ((class (a:ensure-car designator))
            (count (if (consp designator) (second designator) 1)))
        (dotimes (i count)
          (push (make-instance class :owner army) result))))))

(define-condition tile-count-error (ncom:nervous-island-error)
  ((%expected :reader tile-count-error-expected :initarg :expected)
   (%actual :reader tile-count-error-actual :initarg :actual))
  (:default-initargs :actual (a:required-argument :actual)
                     :expected (a:required-argument :expected))
  (:report (lambda (condition stream)
             (format stream "Tile count error in army: expected ~D, but got ~D."
                     (tile-count-error-expected condition)
                     (tile-count-error-actual condition)))))

(defun check-tile-count (hq-tiles tiles tile-count)
  (let ((expected tile-count)
        (actual (+ (length hq-tiles) (length tiles))))
    (unless (= expected actual)
      (error 'tile-count-error :expected expected :actual actual))))

(defmethod shared-initialize :after
    ((army army) slots &key
                         (tile-count nil tile-count-p)
                         (hq-tiles nil hq-tiles-p)
                         (tiles nil tilesp))
  (when (or tile-count-p hq-tiles-p tilesp)
    (let* ((hq-tiles (if hq-tiles-p
                         (process-tile-designators hq-tiles army)
                         (hq-tiles army)))
           (tiles (if tilesp
                      (process-tile-designators tiles army)
                      (tiles army)))
           (tile-count (if tile-count-p tile-count (tile-count army))))
      (check-tile-count hq-tiles tiles tile-count)
      (when hq-tiles-p
        (setf (slot-value army '%hq-tiles) hq-tiles))
      (when tilesp
        (setf (slot-value army '%tiles) tiles)))))
