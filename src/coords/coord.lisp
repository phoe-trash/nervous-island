;;;; src/coords/coord.lisp

(uiop:define-package #:nervous-island.coord
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:ncom #:nervous-island.common))
  (:export
   #:coord #:axial #:axial-q #:axial-r #:cube #:cube-x #:cube-y #:cube-z
   #:cube-axial #:axial-cube #:axial+ #:axial- #:axial-position #:cube+
   #:cube- #:axial-move #:cube-move #:cube-position #:ensure-axial
   #:neighbors #:diagonals #:range #:distance #:range-intersection
   #:rotate-axial #:rotate #:ring #:spiral-ring #:cube-round #:axial-round
   #:lerp #:linedraw))

(in-package #:nervous-island.coord)

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
;;; Operations

(defun neighbors (axial)
  (check-type axial axial)
  (let ((q (axial-q axial))
        (r (axial-r axial)))
    (list (make-axial (1+ q) r) (make-axial (1+ q) (1- r))
          (make-axial q (1- r)) (make-axial (1- q) r)
          (make-axial (1- q) (1+ r)) (make-axial q (1+ r)))))

(defun diagonals (axial)
  (check-type axial axial)
  (let ((q (axial-q axial))
        (r (axial-r axial)))
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
  (if (equalp start end)
      0
      (let ((q1 (axial-q start))
            (r1 (axial-r start))
            (q2 (axial-q end))
            (r2 (axial-r end)))
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

(defun rotate (axial center rotation)
  (let* ((shifted (axial- axial center))
         (rotated (rotate-axial shifted (mod rotation 6))))
    (axial+ rotated center)))

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

(defun %lerp (a b v)
  (+ a (* (- b a) v)))

(defun lerp (cube-1 cube-2 v)
  (list (%lerp (cube-x cube-1) (cube-x cube-2) v)
        (%lerp (cube-y cube-1) (cube-y cube-2) v)
        (%lerp (cube-z cube-1) (cube-z cube-2) v)))

(defun linedraw (axial-1 axial-2)
  (if (equalp axial-1 axial-2)
      (list axial-1)
      (let ((distance (distance axial-1 axial-2))
            (cube-1 (axial-cube axial-1))
            (cube-2 (axial-cube axial-2)))
        (loop for n to distance
              for fractions = (lerp cube-1 cube-2 (/ n distance))
              collect (cube-axial (apply #'cube-round fractions))))))
