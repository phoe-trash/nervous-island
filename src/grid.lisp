;;;; src/grid.lisp

(uiop:define-package #:nervous-island.grid
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:direction #:*directions* #:diagonal #:diagonals
   #:coord #:axial #:axial-q #:axial-r #:cube #:cube-x #:cube-y #:cube-z
   #:cube-axial #:axial-cube #:axial+ #:axial- #:axial-position #:cube+
   #:cube- #:axial-move #:cube-move #:cube-position #:ensure-axial
   #:board #:make-board #:axial-present-p #:axial #:only-present-axials
   #:neighbors #:neighbor #:diagonals #:diagonal #:range #:distance
   #:range-intersection #:rotate #:ring #:spiral-ring #:linedraw #:pathfind))

(in-package #:nervous-island.grid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types and constants

;;; TODO move to nervous-island.common

(deftype direction () '(member :q :w :e :d :s :a))

(defparameter *directions* '(:q :w :e :d :s :a))

(deftype diagonal () '(member :qw :wq :we :ew :ed :de :ds :sd :as :sa :aq :qa))

(defparameter *diagonals* '(:qw :wq :we :ew :ed :de :ds :sd :as :sa :aq :qa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates

(defstruct (coord (:constructor nil) (:copier nil) (:predicate nil)))

(defstruct (axial (:constructor %make-axial) (:include coord))
  (q 0 :type integer :read-only t)
  (r 0 :type integer :read-only t))

(defun make-axial (q r)
  (%make-axial :q q :r r))

(defmethod print-object ((object axial) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~D ~D" (axial-q object) (axial-r object))))

(defstruct (cube (:constructor %make-cube) (:include coord))
  (x 0 :type integer :read-only t)
  (y 0 :type integer :read-only t)
  (z 0 :type integer :read-only t))

(defun make-cube (x y z)
  (assert (= 0 (+ x y z)))
  (%make-cube :x x :y y :z z))

(defmethod print-object ((object cube) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~D ~D ~D" (cube-x object) (cube-y object) (cube-z object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinate conversion and arithmetic

(defun cube-axial (cube)
  (let ((q (cube-x cube))
        (r (cube-z cube)))
    (make-axial q r)))

(defun axial-cube (axial)
  (let* ((x (axial-q axial))
         (z (axial-r axial))
         (y (- 0 x z)))
    (make-cube x y z)))

(defgeneric axial+ (a b)
  (:method ((a axial) (b axial))
    (make-axial (+ (axial-q a) (axial-q b))
                (+ (axial-r a) (axial-r b)))))

(defgeneric axial- (a b)
  (:method ((a axial) (b axial))
    (make-axial (- (axial-q a) (axial-q b))
                (- (axial-r a) (axial-r b)))))

(deftype axial-position () '(cons integer (cons integer null)))

(defgeneric cube+ (a b)
  (:method ((a cube) (b cube))
    (make-cube (+ (cube-x a) (cube-x b))
               (+ (cube-y a) (cube-y b))
               (+ (cube-z a) (cube-z b)))))

(defgeneric cube- (a b)
  (:method ((a cube) (b cube))
    (make-cube (- (cube-x a) (cube-x b))
               (- (cube-y a) (cube-y b))
               (- (cube-z a) (cube-z b)))))

(defgeneric axial-move (axial direction)
  (:method ((axial axial) direction)
    (axial+ axial (apply #'make-axial
                         (ecase direction
                           (:q '(-1 0)) (:w '(0 -1)) (:e '(1 -1))
                           (:a '(-1 1)) (:s '( 0 1)) (:d '(1 0))
                           ((:qw :wq) '(-1 -1)) ((:we :ew) '(1 -2))
                           ((:ed :de) '(2 -1)) ((:sd :ds) '(1 1))
                           ((:as :sa) '(-1 2)) ((:aq :qa) '(-2 1)))))))

(defgeneric cube-move (cube direction)
  (:method ((cube cube) direction)
    (axial-cube (axial-move (cube-axial cube) direction))))

(deftype cube-position () '(cons integer (cons integer (cons integer null))))

(defun ensure-axial (thing)
  (etypecase thing
    (axial thing)
    (axial-position (apply #'make-axial thing))
    (cube (cube-axial thing))
    (cube-position (cube-axial (apply #'make-cube thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Board

(defclass board ()
  ((%axials :reader axials :initform (make-hash-table :test #'equalp))))

(defmethod print-object ((object board) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D axials)" (hash-table-count (axials object)))))

(defmethod initialize-instance :after ((board board) &key axials)
  (dolist (axial axials)
    (check-type axial axial)
    (if (gethash axial (axials board))
        (error "Duplicated axial ~S" axial)
        (setf (gethash axial (axials board)) axial))))

(defun make-board (&rest axial-designators)
  (let ((axials (mapcar #'ensure-axial axial-designators)))
    (make-instance 'board :axials axials)))

(defgeneric axial-present-p (board axial)
  (:method ((board board) (axial axial))
    (gethash axial (axials board))))

(defun error-axial-not-found (axial board)
  (error "No axial ~S in board ~S." axial board))

(defgeneric axial (board coord)
  (:method ((board board) (cube cube))
    (axial board (cube-axial cube)))
  (:method ((board board) (axial axial))
    (multiple-value-bind (value foundp) (gethash axial (axials board))
      (if foundp value (error-axial-not-found axial board)))))

(defgeneric check-axial-present (board axial)
  (:method ((board board) (axial axial))
    (unless (gethash axial (axials board))
      (error-axial-not-found axial board))))

(defgeneric only-present-axials (board axials)
  (:method ((board board) axials)
    (loop for axial in axials
          when (gethash axial (axials board)) collect axial)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Neighbors

(defun %neighbors (axial)
  (check-type axial axial)
  (let ((q (axial-q axial))
        (r (axial-r axial)))
    (list (make-axial (1+ q) r) (make-axial (1+ q) (1- r))
          (make-axial q (1- r)) (make-axial (1- q) r)
          (make-axial (1- q) (1+ r)) (make-axial q (1+ r)))))

(defgeneric neighbors (board axial)
  (:method ((board null) (axial axial))
    (%neighbors axial))
  (:method ((board board) (axial axial))
    (check-axial-present board axial)
    (only-present-axials board (neighbors nil axial))))

(defgeneric neighbor (board axial direction)
  (:method ((board null) (axial axial) direction)
    (axial-move axial direction))
  (:method ((board board) (axial axial) direction)
    (check-axial-present board axial)
    (gethash (axial-move axial direction) (axials board))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diagonals

(defun %diagonals (axial)
  (check-type axial axial)
  (let ((q (axial-q axial))
        (r (axial-r axial)))
    (flet ((2+ (x) (+ x 2))
           (2- (x) (- x 2)))
      (list (make-axial (2+ q) (1- r)) (make-axial (1+ q) (1+ r))
            (make-axial (1- q) (2+ r)) (make-axial (2- q) (1+ r))
            (make-axial (1- q) (1- r)) (make-axial (1+ q) (2- r))))))

(defgeneric diagonals (board axial)
  (:method ((board null) (axial axial))
    (%diagonals axial))
  (:method ((board board) (axial axial))
    (check-axial-present board axial)
    (only-present-axials board (diagonals nil axial))))

(defgeneric diagonal (board axial direction)
  (:method ((board null) (axial axial) direction)
    (axial-move axial direction))
  (:method ((board board) (axial axial) direction)
    (check-axial-present board axial)
    (gethash (axial-move axial direction) (axials board))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Range

(defun %range (center radius)
  (check-type radius unsigned-byte)
  (loop for x from (- radius) to radius
        nconc (loop for y from (max (- radius) (- (+ x radius)))
                      to (min radius (- (- x radius)))
                    for z = (- (+ x y))
                    for result-cube = (make-cube x y z)
                    for result-axial = (cube-axial result-cube)
                    collect (axial+ center result-axial))))

(defgeneric range (board center radius)
  (:method ((board null) (center axial) radius)
    (%range center radius))
  (:method ((board board) (center axial) radius)
    (check-axial-present board center)
    (only-present-axials board (range nil center radius))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Distance

(defun %simple-distance (start end)
  (if (equalp start end)
      0
      (let ((q1 (axial-q start))
            (r1 (axial-r start))
            (q2 (axial-q end))
            (r2 (axial-r end)))
        (* 1/2 (+ (abs (- q1 q2))
                  (abs (+ q1 r1 (- q2) (- r2)))
                  (abs (- r1 r2)))))))

(defun %bfs-distance (board start end max-depth)
  (if (equalp start end)
      0
      (let ((visited (make-hash-table :test #'equalp))
            (fringes (make-array 0 :adjustable t)))
        (setf (gethash start visited) t)
        (vector-push-extend (list start) fringes)
        (loop for k from 1 to max-depth
              do (vector-push-extend '() fringes)
                 (loop for fringe in (aref fringes (1- k))
                       for neighbors = (neighbors board fringe)
                       do (dolist (neighbor neighbors)
                            (cond ((equalp neighbor end)
                                   (return-from %bfs-distance k))
                                  ((not (gethash neighbor visited))
                                   (setf (gethash neighbor visited) t)
                                   (push neighbor (aref fringes k))))))))))

(defgeneric distance (board start end &optional max-depth)
  (:method ((board null) (start axial) (end axial) &optional (max-depth 20))
    (declare (ignore max-depth))
    (%simple-distance start end))
  (:method ((board board) (start axial) (end axial) &optional (max-depth 20))
    (check-axial-present board start)
    (check-axial-present board end)
    (%bfs-distance board start end max-depth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Range intersection

(defun %range-intersection (axial-1 range-1 axial-2 range-2)
  (check-type range-1 unsigned-byte)
  (check-type range-2 unsigned-byte)
  (let ((cube-1 (axial-cube axial-1))
        (cube-2 (axial-cube axial-2)))
    (flet ((compute-min (fn) (max (- (funcall fn cube-1) range-1)
                                  (- (funcall fn cube-2) range-2)))
           (compute-max (fn) (min (+ (funcall fn cube-1) range-1)
                                  (+ (funcall fn cube-2) range-2))))
      (loop with xmin = (compute-min #'cube-x)
            with xmax = (compute-max #'cube-x)
            with ymin = (compute-min #'cube-y)
            with zmin = (compute-min #'cube-z)
            with ymax = (compute-max #'cube-y)
            with zmax = (compute-max #'cube-z)
            for x from xmin to xmax
            nconc (loop for y from (max ymin (- (+ x zmax)))
                          to (min ymax (- (+ x zmin)))
                        for z = (- (+ x y))
                        for result-cube = (make-cube x y z)
                        collect (cube-axial result-cube))))))

(defgeneric range-intersection (board center-1 range-1 center-2 range-2)
  (:method ((board null) (center-1 axial) range-1 (center-2 axial) range-2)
    (%range-intersection center-1 range-1 center-2 range-2))
  (:method ((board board) (center-1 axial) range-1 (center-2 axial) range-2)
    (check-axial-present board center-1)
    (check-axial-present board center-2)
    (only-present-axials board (range-intersection nil center-1 range-1
                                                   center-2 range-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rotation

(defun rotate-axial (axial clockwise-turns)
  (let* ((cube (axial-cube axial))
         (x (cube-x cube)) (y (cube-y cube)) (z (cube-z cube))
         (result-cube (ecase clockwise-turns
                        (0 cube)
                        (1 (make-cube (- z) (- x) (- y)))
                        (2 (make-cube (+ y) (+ z) (+ x)))
                        (3 (make-cube (- x) (- y) (- z)))
                        (4 (make-cube (+ z) (+ x) (+ y)))
                        (5 (make-cube (- y) (- z) (- x))))))
    (cube-axial result-cube)))

(defun %rotate (axial center rotation)
  (let* ((shifted (axial- axial center))
         (rotated (rotate-axial shifted (mod rotation 6))))
    (axial+ rotated center)))

(defgeneric rotate (board axial center rotation)
  (:method ((board null) (axial axial) (center axial) rotation)
    (%rotate axial center rotation))
  (:method ((board board) (axial axial) (center axial) rotation)
    (check-axial-present board axial)
    (check-axial-present board center)
    (let ((rotated (rotate nil axial center rotation)))
      (gethash rotated (axials board)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rings

(defun %ring (center radius)
  (if (= radius 0)
      (list center)
      (let ((result '())
            (axial (axial+ center (make-axial 0 radius))))
        (dolist (direction *directions* (nreverse result))
          (dotimes (i radius)
            (push axial result)
            (setf axial (axial-move axial direction)))))))

(defgeneric ring (board center radius)
  (:method ((board null) (center axial) radius)
    (%ring center radius))
  (:method ((board board) (center axial) radius)
    (check-axial-present board center)
    (only-present-axials board (ring nil center radius))))

(defun %spiral-ring (center radius)
  (loop for k from 0 to radius
        nconc (%ring center k) into result
        finally (return result)))

(defgeneric spiral-ring (board center radius)
  (:method ((board null) (center axial) radius)
    (%spiral-ring center radius))
  (:method ((board board) (center axial) radius)
    (check-axial-present board center)
    (only-present-axials board (spiral-ring nil center radius))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rounding

(defun %round (number)
  (if (= 1/2 (mod number 1))
      (truncate number)
      (round number)))

(defun cube-round (fx fy fz)
  (let* ((x (%round fx)) (dx (abs (- x fx)))
         (y (%round fy)) (dy (abs (- y fy)))
         (z (%round fz)) (dz (abs (- z fz))))
    (cond ((= dx (max dx dy dz)) (setf x (- (+ y z))))
          ((= dy (max dx dy dz)) (setf y (- (+ x z))))
          ((= dz (max dx dy dz)) (setf z (- (+ x y)))))
    (make-cube x y z)))

(defun axial-round (fq fr)
  (let* ((fx fq)
         (fz fr)
         (fy (- 0 fx fz)))
    (cube-axial (cube-round fx fy fz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Line drawing

(defun lerp (a b v)
  (+ a (* (- b a) v)))

(defun cube-lerp (cube-1 cube-2 v)
  (list (lerp (cube-x cube-1) (cube-x cube-2) v)
        (lerp (cube-y cube-1) (cube-y cube-2) v)
        (lerp (cube-z cube-1) (cube-z cube-2) v)))

(defun %linedraw (cube-1 cube-2)
  (if (equalp cube-1 cube-2)
      (list cube-1)
      (let ((distance (%simple-distance (cube-axial cube-1)
                                        (cube-axial cube-2))))
        (loop for n to distance
              collect (apply #'cube-round (cube-lerp cube-1 cube-2
                                                     (/ n distance)))))))

(defgeneric linedraw (axial-1 axial-2)
  (:method ((axial-1 axial) (axial-2 axial))
    (mapcar #'cube-axial (%linedraw (axial-cube axial-1)
                                    (axial-cube axial-2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pathfinding

(defun %simple-pathfind (start end)
  (mapcar #'ensure-axial (%cube-linedraw (axial-cube start) (axial-cube end))))

(defun %bfs-pathfind (board start end max-depth)
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
                       (return-from %bfs-pathfind
                         (loop for elt = end then (gethash elt previous)
                               while elt collect elt into result
                               finally (return (nreverse result))))
                  when (not (gethash neighbor visited))
                    do (setf (gethash neighbor visited) t
                             (gethash neighbor previous) fringe)
                       (push neighbor (aref fringes k))))))))

(defgeneric pathfind (board start end &optional max-depth)
  (:method ((board null) (start axial) (end axial) &optional (max-depth 20))
    (declare (ignore max-depth))
    (%simple-pathfind start end))
  (:method ((board board) (start axial) (end axial) &optional (max-depth 20))
    (check-axial-present board start)
    (check-axial-present board end)
    (%bfs-pathfind board start end max-depth)))
