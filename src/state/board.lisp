;;;; src/state/board.lisp

(uiop:define-package #:nervous-island.board
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:nc #:nervous-island.coord)
                    (#:ncom #:nervous-island.common))
  (:export
   #:board #:axials #:duplicated-axial #:make-board #:axial-present-p
   #:only-present-axials
   #:neighbors #:neighbor #:diagonals #:diagonal #:range #:distance
   #:range-intersection #:rotate #:ring #:spiral-ring #:pathfind))

(in-package #:nervous-island.board)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Board

(defclass board ()
  ((%axials :reader axials :initform (make-hash-table :test #'equalp))))

(defmethod print-object ((object board) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D axials)" (hash-table-count (axials object)))))

(define-condition duplicated-axial (error)
  ((%axial :reader duplicated-axial-axial :initarg :axial))
  (:default-initargs :axial (a:required-argument :axial))
  (:report (lambda (condition stream)
             (format stream "Duplicated axial ~S"
                     (duplicated-axial-axial condition)))))

(defmethod shared-initialize ((board board) slots &key axials)
  (call-next-method)
  (dolist (axial axials)
    (check-type axial nc:axial)
    (if (gethash axial (axials board))
        (error 'duplicated-axial :axial axial)
        (setf (gethash axial (axials board)) axial))))

(defun make-board (&rest axial-designators)
  (let ((axials (mapcar #'nc:ensure-axial axial-designators)))
    (make-instance 'board :axials axials)))

(defgeneric axial-present-p (board axial)
  (:method ((board board) (axial nc:axial))
    (gethash axial (axials board))))

(defun error-axial-not-found (axial board)
  (error "No axial ~S in board ~S." axial board))

(defgeneric check-axial-present (board axial)
  (:method ((board board) (axial nc:axial))
    (unless (gethash axial (axials board))
      (error-axial-not-found axial board))))

(defgeneric only-present-axials (board axials)
  (:method ((board board) axials)
    (loop for axial in axials
          when (gethash axial (axials board)) collect axial)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations

(defgeneric neighbors (board axial)
  (:method ((board board) (axial nc:axial))
    (check-axial-present board axial)
    (only-present-axials board (nc:neighbors axial))))

(defgeneric neighbor (board axial direction)
  (:method ((board board) (axial nc:axial) direction)
    (check-axial-present board axial)
    (check-type direction ncom:direction)
    (gethash (nc:axial-move axial direction) (axials board))))

(defgeneric diagonals (board axial)
  (:method ((board board) (axial nc:axial))
    (check-axial-present board axial)
    (only-present-axials board (nc:diagonals axial))))

(defgeneric diagonal (board axial direction)
  (:method ((board board) (axial nc:axial) direction)
    (check-axial-present board axial)
    (check-type direction ncom:diagonal)
    (gethash (nc:axial-move axial direction) (axials board))))

(defgeneric range (board center radius)
  (:method ((board board) (center nc:axial) radius)
    (check-axial-present board center)
    (only-present-axials board (nc:range center radius))))

(defgeneric range-intersection (board center-1 range-1 center-2 range-2)
  (:method ((board board)
            (center-1 nc:axial) range-1
            (center-2 nc:axial) range-2)
    (check-axial-present board center-1)
    (check-axial-present board center-2)
    (only-present-axials board (nc:range-intersection center-1 range-1
                                                      center-2 range-2))))

(defgeneric rotate (board axial center rotation)
  (:method ((board board) (axial nc:axial) (center nc:axial) rotation)
    (check-axial-present board axial)
    (check-axial-present board center)
    (gethash (nc:rotate axial center rotation) (axials board))))

(defgeneric ring (board center radius)
  (:method ((board board) (center nc:axial) radius)
    (check-axial-present board center)
    (only-present-axials board (nc:ring center radius))))

(defgeneric spiral-ring (board center radius)
  (:method ((board board) (center nc:axial) radius)
    (check-axial-present board center)
    (only-present-axials board (nc:spiral-ring center radius))))

(defgeneric distance (board start end &optional max-depth)
  (:method ((board board) (start nc:axial) (end nc:axial)
            &optional (max-depth 20))
    (check-axial-present board start)
    (check-axial-present board end)
    (if (equalp start end)
        0
        (let ((visited (make-hash-table :test #'equalp))
              (fringes (make-array 0 :adjustable t)))
          (setf (gethash start visited) t)
          (vector-push-extend (list start) fringes)
          (loop for k from 1 to max-depth do
            (vector-push-extend '() fringes)
            (loop for fringe in (aref fringes (1- k)) do
              (loop for neighbor in (neighbors board fringe)
                    when (equalp neighbor end)
                      do (return-from distance k)
                    when (not (gethash neighbor visited))
                      do (setf (gethash neighbor visited) t)
                         (push neighbor (aref fringes k)))))))))

(defgeneric pathfind (board start end &optional max-depth)
  (:method ((board board) (start nc:axial) (end nc:axial)
            &optional (max-depth 20))
    (check-axial-present board start)
    (check-axial-present board end)
    (if (equalp start end)
        (list start)
        (let ((visited (make-hash-table :test #'equalp))
              (previous (make-hash-table :test #'equalp))
              (fringes (make-array 0 :adjustable t)))
          (setf (gethash start visited) t)
          (vector-push-extend (list start) fringes)
          (loop for k from 1 to max-depth do
            (vector-push-extend '() fringes)
            (loop for fringe in (aref fringes (1- k)) do
              (loop for neighbor in (neighbors board fringe)
                    when (equalp neighbor end)
                      do (setf (gethash neighbor previous) fringe)
                         (return-from pathfind
                           (loop for elt = end then (gethash elt previous)
                                 while elt collect elt into result
                                 finally (return (nreverse result))))
                    when (not (gethash neighbor visited))
                      do (setf (gethash neighbor visited) t
                               (gethash neighbor previous) fringe)
                         (push neighbor (aref fringes k)))))))))
