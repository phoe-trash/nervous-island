;;;; src/state/coord.lisp

(uiop:define-package #:nervous-island.coord
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:ncom #:nervous-island.common))
  (:export
   #:coord #:axial #:q #:r #:cube #:x #:y #:z
   #:make-axial #:make-cube #:invalid-coords #:cube-axial #:axial-cube
   #:axial+ #:axial- #:cube+ #:cube- #:axial-move #:cube-move
   #:axial-position #:cube-position #:ensure-axial #:ensure-cube
   #:neighbors #:diagonals #:range #:distance #:range-intersection
   #:rotate #:ring #:spiral-ring #:cube-round #:axial-round #:lerp #:linedraw))

(in-package #:nervous-island.coord)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates

(define-class coord () ()
  (:protocolp t))

(define-class axial (coord)
  ((q :type integer :initform 0)
   (r :type integer :initform 0)))

(defun make-axial (q r)
  (make-instance 'axial :q q :r r))

(defmethod print-object ((object axial) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~D ~D" (q object) (r object))))

(define-class cube (coord)
  ((x :type integer :initform 0)
   (y :type integer :initform 0)
   (z :type integer :initform 0)))

(define-condition invalid-coords (ncom:nervous-island-error)
  ((%coordinates :reader invalid-coords-coords :initarg :coords))
  (:default-initargs :coords (a:required-argument :coords))
  (:report (lambda (condition stream)
             (format stream "Invalid coordinates: ~S"
                     (invalid-coords-coords condition)))))

(defmethod shared-initialize :after ((cube cube) slot-names &key)
  (let ((x (x cube))
        (y (y cube))
        (z (z cube)))
    (unless (= 0 (+ x y z))
      (error 'invalid-coords :coords (list x y z)))))

(defun make-cube (x y z)
  (make-instance 'cube :x x :y y :z z))

(defmethod print-object ((object cube) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~D ~D ~D" (x object) (y object) (z object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinate conversion and arithmetic

(defun cube-axial (cube)
  (let ((q (x cube))
        (r (z cube)))
    (make-axial q r)))

(defun axial-cube (axial)
  (let* ((x (q axial))
         (z (r axial))
         (y (- 0 x z)))
    (make-cube x y z)))

(defun axial+ (a b)
  (make-axial (+ (q a) (q b))
              (+ (r a) (r b))))

(defun axial- (a b)
  (make-axial (- (q a) (q b))
              (- (r a) (r b))))

(defun cube+ (a b)
  (make-cube (+ (x a) (x b))
             (+ (y a) (y b))
             (+ (z a) (z b))))

(defun cube- (a b)
  (make-cube (- (x a) (x b))
             (- (y a) (y b))
             (- (z a) (z b))))

(defun axial-move (axial direction)
  (axial+ axial (apply #'make-axial
                       (ecase direction
                         (:q '(-1 0)) (:w '(0 -1)) (:e '(1 -1))
                         (:a '(-1 1)) (:s '( 0 1)) (:d '(1 0))
                         (:qw '(-1 -1)) (:we '(1 -2))
                         (:ed '(2 -1)) (:ds '(1 1))
                         (:sa '(-1 2)) (:aq '(-2 1))))))

(defun cube-move (cube direction)
  (axial-cube (axial-move (cube-axial cube) direction)))

(deftype axial-position () '(cons integer (cons integer null)))

(deftype cube-position ()
  '(and (cons integer (cons integer (cons integer null)))
    (satisfies cube-position-good-coords)))

(defun cube-position-good-coords (position)
  (destructuring-bind (x y z) position
    (= 0 (+ x y z))))

(defun ensure-axial (thing)
  (etypecase thing
    (axial thing)
    (axial-position (apply #'make-axial thing))
    (cube (cube-axial thing))
    (cube-position (cube-axial (apply #'make-cube thing)))))

(defun ensure-cube (thing)
  (etypecase thing
    (cube thing)
    (cube-position (apply #'make-cube thing))
    (axial (axial-cube thing))
    (axial-position (axial-cube (apply #'make-axial thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations

(defun neighbors (axial)
  (check-type axial axial)
  (let ((q (q axial))
        (r (r axial)))
    (list (make-axial (1+ q) r) (make-axial (1+ q) (1- r))
          (make-axial q (1- r)) (make-axial (1- q) r)
          (make-axial (1- q) (1+ r)) (make-axial q (1+ r)))))

(defun diagonals (axial)
  (check-type axial axial)
  (let ((q (q axial))
        (r (r axial)))
    (flet ((2+ (x) (+ x 2))
           (2- (x) (- x 2)))
      (list (make-axial (2+ q) (1- r)) (make-axial (1+ q) (1+ r))
            (make-axial (1- q) (2+ r)) (make-axial (2- q) (1+ r))
            (make-axial (1- q) (1- r)) (make-axial (1+ q) (2- r))))))

(defun range (center radius)
  (check-type radius unsigned-byte)
  (loop for x from (- radius) to radius
        nconc (loop for y from (max (- radius) (- (+ x radius)))
                      to (min radius (- (- x radius)))
                    for z = (- (+ x y))
                    for result-cube = (make-cube x y z)
                    for result-axial = (cube-axial result-cube)
                    collect (axial+ center result-axial))))

(defun distance (start end)
  (if (eqv start end)
      0
      (let ((q1 (q start))
            (r1 (r start))
            (q2 (q end))
            (r2 (r end)))
        (* 1/2 (+ (abs (- q1 q2))
                  (abs (+ q1 r1 (- q2) (- r2)))
                  (abs (- r1 r2)))))))

(defun range-intersection (axial-1 range-1 axial-2 range-2)
  (check-type range-1 unsigned-byte)
  (check-type range-2 unsigned-byte)
  (let ((cube-1 (axial-cube axial-1))
        (cube-2 (axial-cube axial-2)))
    (flet ((compute-min (fn) (max (- (funcall fn cube-1) range-1)
                                  (- (funcall fn cube-2) range-2)))
           (compute-max (fn) (min (+ (funcall fn cube-1) range-1)
                                  (+ (funcall fn cube-2) range-2))))
      (loop with xmin = (compute-min #'x)
            with xmax = (compute-max #'x)
            with ymin = (compute-min #'y)
            with zmin = (compute-min #'z)
            with ymax = (compute-max #'y)
            with zmax = (compute-max #'z)
            for x from xmin to xmax
            nconc (loop for y from (max ymin (- (+ x zmax)))
                          to (min ymax (- (+ x zmin)))
                        for z = (- (+ x y))
                        for result-cube = (make-cube x y z)
                        collect (cube-axial result-cube))))))

(defun rotate-axial (axial rotation)
  (let* ((cube (axial-cube axial))
         (x (x cube)) (y (y cube)) (z (z cube))
         (result-cube (ecase (mod rotation 6)
                        (0 cube)
                        (1 (make-cube (- z) (- x) (- y)))
                        (2 (make-cube (+ y) (+ z) (+ x)))
                        (3 (make-cube (- x) (- y) (- z)))
                        (4 (make-cube (+ z) (+ x) (+ y)))
                        (5 (make-cube (- y) (- z) (- x))))))
    (cube-axial result-cube)))

(defun rotate (axial center rotation)
  (let* ((shifted (axial- axial center))
         (rotated (rotate-axial shifted (mod rotation 6)))
         (unshifted (axial+ rotated center)))
    unshifted))

(defun ring (center radius)
  (if (= radius 0)
      (list center)
      (let ((result '())
            (axial (axial+ center (make-axial 0 radius))))
        (dolist (direction ncom:*directions* (nreverse result))
          (dotimes (i radius)
            (push axial result)
            (setf axial (axial-move axial direction)))))))

(defun spiral-ring (center radius)
  (loop for k from 0 to radius
        nconc (ring center k) into result
        finally (return result)))

(defun cube-round (fx fy fz)
  (flet ((%round (number)
           (if (= 1/2 (mod number 1)) (truncate number) (round number))))
    (let* ((x (%round fx)) (dx (abs (- x fx)))
           (y (%round fy)) (dy (abs (- y fy)))
           (z (%round fz)) (dz (abs (- z fz))))
      (cond ((= dx (max dx dy dz)) (setf x (- (+ y z))))
            ((= dy (max dx dy dz)) (setf y (- (+ x z))))
            ((= dz (max dx dy dz)) (setf z (- (+ x y)))))
      (make-cube x y z))))

(defun axial-round (fq fr)
  (let* ((fx fq)
         (fz fr)
         (fy (- 0 fx fz)))
    (cube-axial (cube-round fx fy fz))))

(defun %lerp (a b v)
  (+ a (* (- b a) v)))

(defun lerp (cube-1 cube-2 v)
  (list (%lerp (x cube-1) (x cube-2) v)
        (%lerp (y cube-1) (y cube-2) v)
        (%lerp (z cube-1) (z cube-2) v)))

(defun linedraw (axial-1 axial-2)
  (if (eqv axial-1 axial-2)
      (list axial-1)
      (let ((distance (distance axial-1 axial-2))
            (cube-1 (axial-cube axial-1))
            (cube-2 (axial-cube axial-2)))
        (loop for n to distance
              for fractions = (lerp cube-1 cube-2 (/ n distance))
              collect (cube-axial (apply #'cube-round fractions))))))
