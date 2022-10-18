;;;; src/state/board.lisp

(uiop:define-package #:nervous-island.board
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:nc #:nervous-island.coord)
                    (#:ncom #:nervous-island.common)
                    (#:nsp #:nervous-island.space))
  (:export
   #:board #:spaces #:standard-board
   #:find-space #:all-elements #:dimensions
   #:space-not-found #:space-not-found-axial #:space-not-found-board
   #:remove-missing-axials
   #:find-element #:augment
   #:neighbors #:neighbor #:diagonals #:diagonal #:range #:distance
   #:range-intersection #:rotate #:ring #:spiral #:pathfind))

(in-package #:nervous-island.board)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Board

(define-class board ()
  ((spaces :type dict :initform (dict)))
  (:before #'make-board-before))

(defmethod print-object ((object board) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "(~D spaces)" (dict-count (spaces object)))))

(defun make-board-before (board &key (spaces nil spacesp) &allow-other-keys)
  (declare (ignore board))
  (when spacesp
    (check-type spaces dict)
    (dolist (cons (dict-contents spaces))
      (check-type (car cons) nc:axial)
      (check-type (cdr cons) nsp:space))))

(defun board (&rest things)
  (make-instance 'board :spaces (apply #'nsp:spaces things)))

(defun standard-board ()
  (apply #'board (set-contents (nc:range (nc:axial 0 0) 2))))

(defun find-space (board axial)
  (dict-find (spaces board) axial))

(defun all-elements (board)
  (loop for (axial . space) in (dict-contents (spaces board))
        nconc (nsp:all-elements space)))

(defun dimensions (board)
  (let ((contents (dict-contents (spaces board))))
    (if (null contents)
        (list 0 0)
        (loop for (axial . space) in contents
              for q = (nc:q axial) for r = (nc:r axial)
              minimize q into min-q maximize q into max-q
              minimize r into min-r maximize r into max-r
              finally (return (values (list (1+ (- max-q min-q))
                                            (1+ (- max-r min-r)))
                                      (list min-q min-r)
                                      (list max-q max-r)))))))

(define-condition space-not-found (ncom:nervous-island-error)
  ((%axial :reader space-not-found-axial :initarg :axial)
   (%board :reader space-not-found-board :initarg :board))
  (:default-initargs :axial (a:required-argument :axial)
                     :board (a:required-argument :board)
                     :army (a:required-argument :army))
  (:report (lambda (condition stream)
             (format stream
                     "No space at axial ~S in board ~S."
                     (space-not-found-axial condition)
                     (space-not-found-board condition)))))

(defun space-not-found (board axial)
  (error 'space-not-found :board board :axial axial))

(defun check-space-present (board axial)
  (unless (find-space board axial)
    (space-not-found axial board)))

(defun %remove-missing-axials (board axials)
  (remove-if-not (a:curry #'find-space board) axials))

(defun remove-missing-axials (board axials)
  (apply #'set (%remove-missing-axials board (set-contents axials))))

(defun find-element (board element)
  (nsp:find-element (spaces board) element))

(defun augment (board &rest dicts-and-spaces)
  (let ((spaces (apply #'nsp:augment-spaces (spaces board) dicts-and-spaces)))
    (make-instance 'board :spaces spaces)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations

(defun neighbors (board axial)
  (check-space-present board axial)
  (remove-missing-axials board (nc:neighbors axial)))

(defun neighbor (board axial direction)
  (check-space-present board axial)
  (check-type direction ncom:direction)
  (let ((result (nc:axial-move axial direction)))
    (if (find-space board result) result nil)))

(defun diagonals (board axial)
  (check-space-present board axial)
  (remove-missing-axials board (nc:diagonals axial)))

(defun diagonal (board axial direction)
  (check-space-present board axial)
  (check-type direction ncom:diagonal)
  (let ((result (nc:axial-move axial direction)))
    (if (find-space board result) result nil)))

(defun range (board center radius)
  (check-space-present board center)
  (remove-missing-axials board (nc:range center radius)))

(defun range-intersection (board center-1 range-1 center-2 range-2)
  (check-space-present board center-1)
  (check-space-present board center-2)
  (remove-missing-axials board (nc:range-intersection center-1 range-1
                                                      center-2 range-2)))

(defun rotate (board axial center rotation)
  (check-space-present board axial)
  (check-space-present board center)
  (let ((result (nc:rotate axial center rotation)))
    (if (find-space board result) result nil)))

(defun ring (board center radius)
  (check-space-present board center)
  (remove-missing-axials board (nc:ring center radius)))

(defun spiral (board center radius)
  (check-space-present board center)
  (%remove-missing-axials board (nc:spiral center radius)))

(defun distance (board start end &optional (max-depth 20))
  (check-space-present board start)
  (check-space-present board end)
  (if (eqv start end)
      0
      (let ((visited '())
            (fringes (make-array 0 :fill-pointer 0)))
        (push start visited)
        (vector-push-extend (list start) fringes)
        (loop for k from 1 to max-depth do
          (vector-push-extend '() fringes)
          (loop for fringe in (aref fringes (1- k)) do
            (loop for neighbor in (set-contents (neighbors board fringe))
                  when (eqv neighbor end)
                    do (return-from distance k)
                  when (not (member neighbor visited :test #'eqv))
                    do (push neighbor visited)
                       (push neighbor (aref fringes k))))))))

(defun pathfind (board start end &optional (max-depth 20))
  (check-space-present board start)
  (check-space-present board end)
  (if (eqv start end)
      (list start)
      (let ((visited '())
            (previous '())
            (fringes (make-array 0 :fill-pointer 0)))
        (push start visited)
        (vector-push-extend (list start) fringes)
        (loop for k from 1 to max-depth do
          (vector-push-extend '() fringes)
          (loop for fringe in (aref fringes (1- k)) do
            (loop for neighbor in (set-contents (neighbors board fringe))
                  when (eqv neighbor end)
                    do (setf (a:assoc-value previous neighbor) fringe)
                       (return-from pathfind
                         (loop for elt = end
                                 then (a:assoc-value previous elt :test #'eqv)
                               while elt collect elt into result
                               finally (return (nreverse result))))
                  when (not (member neighbor visited :test #'eqv))
                    do (push neighbor visited)
                       (setf (a:assoc-value previous neighbor) fringe)
                       (push neighbor (aref fringes k))))))))
