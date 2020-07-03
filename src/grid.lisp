;;;; src/grid.lisp

(defpackage #:nervous-island.grid
  (:use #:cl)
  (:shadow #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base))
  (:export
   #:tile #:tile= #:direction #:*directions* #:diagonal #:diagonals
   #:board-tile #:instant-tile
   #:coord #:axial #:axial-q #:axial-r #:cube #:cube-x #:cube-y #:cube-z
   #:cube-axial #:axial-cube #:axial+ #:axial- #:cube+
   #:cube- #:axial-move #:cube-move
   #:space #:tiles #:coords #:make-space #:ensure-space #:space= #:ensure-space
   #:board #:make-board #:spaces #:space-present-p #:space #:only-present-spaces
   #:neighbors #:neighbor #:diagonals #:diagonal #:range #:distance
   #:range-intersection #:rotate #:ring #:spiral-ring #:linedraw #:pathfind))

(in-package #:nervous-island.grid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types and constants

(deftype direction () '(member :q :w :e :d :s :a))

(defparameter *directions* '(:q :w :e :d :s :a))

(deftype diagonal () '(member :qw :wq :we :ew :ed :de :ds :sd :as :sa :aq :qa))

(defparameter *diagonals* '(:qw :wq :we :ew :ed :de :ds :sd :as :sa :aq :qa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tile

(p:define-protocol-class tile () ())

(defgeneric tile= (tile-1 tile-2)
  (:method (tile-1 tile-2) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates

(defstruct (coord (:constructor nil) (:copier nil) (:predicate nil)))

(defstruct (axial (:constructor %make-axial) (:include coord))
  (q 0 :type integer :read-only t)
  (r 0 :type integer :read-only t))

(defun axial (q r)
  (%make-axial :q q :r r))

(defmethod print-object ((object axial) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~D ~D" (axial-q object) (axial-r object))))

(defstruct (cube (:constructor %make-cube) (:include coord))
  (x 0 :type integer :read-only t)
  (y 0 :type integer :read-only t)
  (z 0 :type integer :read-only t))

(defun cube (x y z)
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
    (axial q r)))

(defun axial-cube (axial)
  (let* ((x (axial-q axial))
         (z (axial-r axial))
         (y (- 0 x z)))
    (cube x y z)))

(defgeneric axial+ (a b)
  (:method ((a axial) (b axial))
    (axial (+ (axial-q a) (axial-q b))
           (+ (axial-r a) (axial-r b)))))

(defgeneric axial- (a b)
  (:method ((a axial) (b axial))
    (axial (- (axial-q a) (axial-q b))
           (- (axial-r a) (axial-r b)))))

(defgeneric cube+ (a b)
  (:method ((a cube) (b cube))
    (cube (+ (cube-x a) (cube-x b))
          (+ (cube-y a) (cube-y b))
          (+ (cube-z a) (cube-z b)))))

(defgeneric cube- (a b)
  (:method ((a cube) (b cube))
    (cube (- (cube-x a) (cube-x b))
          (- (cube-y a) (cube-y b))
          (- (cube-z a) (cube-z b)))))

(defgeneric axial-move (axial direction)
  (:method ((axial axial) direction)
    (axial+ axial (ecase direction
                    (:q (axial -1 0)) (:w (axial 0 -1)) (:e (axial 1 -1))
                    (:a (axial -1 1)) (:s (axial 0 1)) (:d (axial 1 0))
                    ((:qw :wq) (axial -1 -1)) ((:we :ew) (axial 1 -2))
                    ((:ed :de) (axial 2 -1)) ((:sd :ds) (axial 1 1))
                    ((:as :sa) (axial -1 2)) ((:aq :qa) (axial -2 1))))))

(defgeneric cube-move (cube direction)
  (:method ((cube cube) direction)
    (axial-cube (axial-move (cube-axial cube) direction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Space

(defclass space ()
  ((%tiles :accessor tiles :initarg :tiles)
   (%coords :reader coords :initarg :coords))
  (:default-initargs :tiles '() :coords '()))

(defmethod initialize-instance :before ((space space) &key tiles coords)
  (assert (every (a:rcurry #'typep 'tile) tiles))
  (assert (typep coords 'axial)))

(defun make-space (coords &optional tiles)
  (make-instance 'space :coords coords :tiles tiles))

(defgeneric space= (space-1 space-2)
  (:method (space-1 space-2) nil)
  (:method ((space-1 space) (space-2 space))
    (and (equalp (coords space-1) (coords space-2))
         (let ((tiles-1 (tiles space-1))
               (tiles-2 (tiles space-2)))
           (= (length tiles-1) (length tiles-2))
           (every #'tile= tiles-1 tiles-2)))))

(defmethod print-object ((object space) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (let ((axial (coords object)))
      (format stream "~D ~D" (axial-q axial) (axial-r axial))
      (when (tiles object)
        (format stream " (~D tile~:P)" (list-length (tiles object)))))))

(defun ensure-space (thing)
  (etypecase thing
    (space thing)
    (axial (make-space thing))
    (axial-position (make-space (apply #'axial thing)))
    (cube (make-space (cube-axial thing)))
    (cube-position (make-space (cube-axial (apply #'cube thing))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Board

(defclass board ()
  ((%spaces :reader spaces :initform (make-hash-table :test #'equalp))))

(defmethod print-object ((object board) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D spaces)" (hash-table-count (spaces object)))))

(deftype axial-position () '(cons integer (cons integer null)))

(deftype cube-position () '(cons integer (cons integer (cons integer null))))

(defmethod initialize-instance :after ((board board) &key spaces)
  (dolist (space spaces)
    (let ((coords (coords space)))
      (if (gethash coords (spaces board))
          (error "Duplicated space at ~S" coords)
          (setf (gethash coords (spaces board)) space)))))

(defun make-board (&rest space-designators)
  (let ((spaces (mapcar #'ensure-space space-designators)))
    (make-instance 'board :spaces spaces)))

(defgeneric space-present-p (board space)
  (:method ((board board) (space space))
    (gethash (coords space) (spaces board))))

(defun error-space-not-found (coords board)
  (error "No space with coordinates ~S in board ~S." coords board))

(defgeneric space (board coords)
  (:method ((board board) (coords cube))
    (space board (cube-axial coords)))
  (:method ((board board) (coords axial))
    (multiple-value-bind (value foundp) (gethash coords (spaces board))
      (if foundp value (error-space-not-found coords board)))))

(defgeneric check-space-present (board space)
  (:method ((board board) (space space))
    (let ((coords (coords space)))
      (unless (gethash coords (spaces board))
        (error-space-not-found coords board)))))

(defgeneric only-present-spaces (board spaces)
  (:method ((board board) spaces)
    (loop for space in spaces
          when (gethash (coords space) (spaces board)) collect space)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Neighbors

(defun %neighbors (axial)
  (check-type axial axial)
  (let ((q (axial-q axial))
        (r (axial-r axial)))
    (list (axial (1+ q) r) (axial (1+ q) (1- r)) (axial q (1- r))
          (axial (1- q) r) (axial (1- q) (1+ r)) (axial q (1+ r)))))

(defgeneric neighbors (board space)
  (:method ((board null) (space space))
    (mapcar #'make-space (%neighbors (coords space))))
  (:method ((board board) (space space))
    (check-space-present board space)
    (only-present-spaces board (neighbors nil space))))

(defgeneric neighbor (board space direction)
  (:method ((board null) (space space) direction)
    (make-space (axial-move (coords space) direction)))
  (:method ((board board) (space space) direction)
    (check-space-present board space)
    (gethash (axial-move (coords space) direction) (spaces board))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diagonals

(defun %diagonals (axial)
  (check-type axial axial)
  (let ((q (axial-q axial))
        (r (axial-r axial)))
    (flet ((2+ (x) (+ x 2))
           (2- (x) (- x 2)))
      (list (axial (2+ q) (1- r)) (axial (1+ q) (1+ r)) (axial (1- q) (2+ r))
            (axial (2- q) (1+ r)) (axial (1- q) (1- r)) (axial (1+ q) (2- r))))))

(defgeneric diagonals (board space)
  (:method ((board null) (space space))
    (mapcar #'make-space (%diagonals (coords space))))
  (:method ((board board) (space space))
    (check-space-present board space)
    (only-present-spaces board (diagonals nil space))))

(defgeneric diagonal (board space direction)
  (:method ((board null) (space space) direction)
    (make-space (axial-move (coords space) direction)))
  (:method ((board board) (space space) direction)
    (check-space-present board space)
    (gethash (axial-move (coords space) direction) (spaces board))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Range

(defun %range (center radius)
  (check-type radius unsigned-byte)
  (loop for x from (- radius) to radius
        nconc (loop for y from (max (- radius) (- (+ x radius)))
                      to (min radius (- (- x radius)))
                    for z = (- (+ x y))
                    for result-cube = (cube x y z)
                    for result-axial = (cube-axial result-cube)
                    collect (axial+ center result-axial))))

(defgeneric range (board center radius)
  (:method ((board null) (center space) radius)
    (mapcar #'make-space (%range (coords center) radius)))
  (:method ((board board) (center space) radius)
    (check-space-present board center)
    (only-present-spaces board (range nil center radius))))

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
        (setf (gethash (coords start) visited) t)
        (vector-push-extend (list start) fringes)
        (loop for k from 1 to max-depth
              do (vector-push-extend '() fringes)
                 (loop for fringe in (aref fringes (1- k))
                       for neighbors = (neighbors board fringe)
                       do (dolist (neighbor neighbors)
                            (cond ((space= neighbor end)
                                   (return-from %bfs-distance k))
                                  ((not (gethash (coords neighbor) visited))
                                   (setf (gethash (coords neighbor) visited) t)
                                   (push neighbor (aref fringes k))))))))))

(defgeneric distance (board start end &optional max-depth)
  (:method ((board null) (start space) (end space) &optional (max-depth 20))
    (declare (ignore max-depth))
    (%simple-distance (coords start) (coords end)))
  (:method ((board board) (start space) (end space) &optional (max-depth 20))
    (check-space-present board start)
    (check-space-present board end)
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
                        for result-cube = (cube x y z)
                        collect (cube-axial result-cube))))))

(defgeneric range-intersection (board center-1 range-1 center-2 range-2)
  (:method ((board null) (center-1 space) range-1 (center-2 space) range-2)
    (mapcar #'make-space (%range-intersection (coords center-1) range-1
                                              (coords center-2) range-2)))
  (:method ((board board) (center-1 space) range-1 (center-2 space) range-2)
    (check-space-present board center-1)
    (check-space-present board center-2)
    (only-present-spaces board (range-intersection nil center-1 range-1
                                                   center-2 range-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rotation

(defun rotate-axial (axial clockwise-turns)
  (let* ((cube (axial-cube axial))
         (x (cube-x cube)) (y (cube-y cube)) (z (cube-z cube))
         (result-cube (ecase clockwise-turns
                        (0 cube)
                        (1 (cube (- z) (- x) (- y)))
                        (2 (cube (+ y) (+ z) (+ x)))
                        (3 (cube (- x) (- y) (- z)))
                        (4 (cube (+ z) (+ x) (+ y)))
                        (5 (cube (- y) (- z) (- x))))))
    (cube-axial result-cube)))

(defun %rotate (axial center rotation)
  (let* ((shifted (axial- axial center))
         (rotated (rotate-axial shifted (mod rotation 6))))
    (axial+ rotated center)))

(defgeneric rotate (board space center rotation)
  (:method ((board null) (space space) (center space) rotation)
    (make-space (%rotate (coords space) (coords center) rotation)))
  (:method ((board board) (space space) (center space) rotation)
    (check-space-present board space)
    (check-space-present board center)
    (let ((rotated (rotate nil space center rotation)))
      (gethash (coords rotated) (spaces board)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rings

(defun %ring (center radius)
  (if (= radius 0)
      (list center)
      (let ((result '())
            (axial (axial+ center (axial 0 radius))))
        (dolist (direction *directions* (nreverse result))
          (dotimes (i radius)
            (push axial result)
            (setf axial (axial-move axial direction)))))))

(defgeneric ring (board center radius)
  (:method ((board null) (center space) radius)
    (mapcar #'make-space (%ring (coords center) radius)))
  (:method ((board board) (center space) radius)
    (check-space-present board center)
    (only-present-spaces board (ring nil center radius))))

(defun %spiral-ring (center radius)
  (loop for k from 0 to radius
        nconc (%ring center k) into result
        finally (return result)))

(defgeneric spiral-ring (board center radius)
  (:method ((board null) (center space) radius)
    (mapcar #'make-space (%spiral-ring (coords center) radius)))
  (:method ((board board) (center space) radius)
    (check-space-present board center)
    (only-present-spaces board (spiral-ring nil center radius))))

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
    (cube x y z)))

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

(defun %cube-linedraw (cube-1 cube-2)
  (if (equalp cube-1 cube-2)
      (list cube-1)
      (let ((distance (%simple-distance (cube-axial cube-1)
                                        (cube-axial cube-2))))
        (loop for n to distance
              collect (apply #'cube-round (cube-lerp cube-1 cube-2
                                                     (/ n distance)))))))

(defgeneric linedraw (space-1 space-2)
  (:method ((space-1 space) (space-2 space))
    (mapcar #'ensure-space (%cube-linedraw (axial-cube (coords space-1))
                                           (axial-cube (coords space-2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pathfinding

(defun %simple-pathfind (start end)
  (mapcar #'ensure-space (%cube-linedraw (axial-cube (coords start))
                                         (axial-cube (coords end)))))

(defun %bfs-pathfind (board start end max-depth)
  (if (equalp start end)
      (list start)
      (let ((visited (make-hash-table :test #'equalp))
            (previous (make-hash-table :test #'equalp))
            (fringes (make-array 0 :adjustable t)))
        (setf (gethash (coords start) visited) t)
        (vector-push-extend (list start) fringes)
        (loop
          for k from 1 to max-depth
          do (vector-push-extend '() fringes)
             (loop
               for fringe in (aref fringes (1- k))
               for neighbors = (neighbors board fringe)
               do (dolist (neighbor neighbors)
                    (cond
                      ((space= neighbor end)
                       (setf (gethash (coords neighbor) previous) fringe)
                       (return-from %bfs-pathfind
                         (loop for elt = end
                                 then (gethash (coords elt) previous)
                               while elt collect elt into result
                               finally (return (nreverse result)))))
                      ((not (gethash (coords neighbor) visited))
                       (setf (gethash (coords neighbor) visited) t
                             (gethash (coords neighbor) previous) fringe)
                       (push neighbor (aref fringes k))))))))))

(defgeneric pathfind (board start end &optional max-depth)
  (:method ((board null) (start space) (end space) &optional (max-depth 20))
    (declare (ignore max-depth))
    (%simple-pathfind start end))
  (:method ((board board) (start space) (end space) &optional (max-depth 20))
    (check-space-present board start)
    (check-space-present board end)
    (%bfs-pathfind board start end max-depth)))
