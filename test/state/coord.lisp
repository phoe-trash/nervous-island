;;;; test/state/board.lisp

(in-package #:nervous-island.test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates

(define-test coord-instantiation
  (let ((axial (nc:make-axial 1 2)))
    (is = 1 (nc:q axial))
    (is = 2 (nc:r axial)))
  (let ((cube (nc:make-cube 1 2 -3)))
    (is = 1 (nc:x cube))
    (is = 2 (nc:y cube))
    (is = -3 (nc:z cube)))
  (fail (nc:make-axial :a 0) 'type-error)
  (fail (nc:make-axial 0 :a) 'type-error)
  (fail (nc:make-cube :a 0 0) 'type-error)
  (fail (nc:make-cube 0 :a 0) 'type-error)
  (fail (nc:make-cube 0 0 :a) 'type-error)
  (fail (nc:make-cube 1 0 0) 'nc:invalid-coords))

(define-test coord-equality
  (is eqv (nc:make-axial 2 4) (nc:make-axial 2 4))
  (isnt eqv (nc:make-axial 2 4) (nc:make-axial 4 2))
  (is eqv (nc:make-cube 2 4 -6) (nc:make-cube 2 4 -6))
  (isnt eqv (nc:make-cube 2 4 -6) (nc:make-cube 4 2 -6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinate conversion and arithmetic

(define-test coord-conversion
  (let* ((axial (nc:make-axial 2 4))
         (cube (nc:axial-cube axial)))
    (is eqv (nc:make-cube 2 -6 4) cube))
  (let* ((cube (nc:make-cube 2 -6 4))
         (axial (nc:cube-axial cube)))
    (is eqv (nc:make-axial 2 4) axial)))

(define-test coord-arithmetic
  (let ((axial-1 (nc:make-axial 4 0))
        (axial-2 (nc:make-axial 0 4)))
    (is eqv (nc:make-axial 4 4) (nc:axial+ axial-1 axial-2))
    (is eqv (nc:make-axial 4 -4) (nc:axial- axial-1 axial-2)))
  (let ((cube-1 (nc:make-cube 4 -4 0))
        (cube-2 (nc:make-cube 0 -4 4)))
    (is eqv (nc:make-cube 4 -8 4) (nc:cube+ cube-1 cube-2))
    (is eqv (nc:make-cube 4 0 -4) (nc:cube- cube-1 cube-2))))

(define-test coord-axial-move-neighbors
  (let ((axial (nc:make-axial 0 0)))
    (is eqv (nc:make-axial -1 0) (nc:axial-move axial :q))
    (is eqv (nc:make-axial 0 -1) (nc:axial-move axial :w))
    (is eqv (nc:make-axial 1 -1) (nc:axial-move axial :e))
    (is eqv (nc:make-axial -1 1) (nc:axial-move axial :a))
    (is eqv (nc:make-axial 0 1) (nc:axial-move axial :s))
    (is eqv (nc:make-axial 1 0) (nc:axial-move axial :d))))

(define-test coord-axial-move-diagonals
  (let ((axial (nc:make-axial 0 0)))
    (is eqv (nc:make-axial -1 -1) (nc:axial-move axial :qw))
    (is eqv (nc:make-axial 1 -2) (nc:axial-move axial :we))
    (is eqv (nc:make-axial 2 -1) (nc:axial-move axial :ed))
    (is eqv (nc:make-axial -2 1) (nc:axial-move axial :aq))
    (is eqv (nc:make-axial -1 2) (nc:axial-move axial :sa))
    (is eqv (nc:make-axial 1 1) (nc:axial-move axial :ds))))

(define-test coord-cube-move-neighbors
  (let ((cube (nc:make-cube 0 0 0)))
    (is eqv (nc:make-cube -1 1 0) (nc:cube-move cube :q))
    (is eqv (nc:make-cube 0 1 -1) (nc:cube-move cube :w))
    (is eqv (nc:make-cube 1 0 -1) (nc:cube-move cube :e))
    (is eqv (nc:make-cube -1 0 1) (nc:cube-move cube :a))
    (is eqv (nc:make-cube 0 -1 1) (nc:cube-move cube :s))
    (is eqv (nc:make-cube 1 -1 0) (nc:cube-move cube :d))))

(define-test coord-cube-move-diagonals
  (let ((cube (nc:make-cube 0 0 0)))
    (is eqv (nc:make-cube -1 2 -1) (nc:cube-move cube :qw))
    (is eqv (nc:make-cube 1 1 -2) (nc:cube-move cube :we))
    (is eqv (nc:make-cube 2 -1 -1) (nc:cube-move cube :ed))
    (is eqv (nc:make-cube -2 1 1) (nc:cube-move cube :aq))
    (is eqv (nc:make-cube -1 -1 2) (nc:cube-move cube :sa))
    (is eqv (nc:make-cube 1 -2 1) (nc:cube-move cube :ds))))

(define-test coord-types
  (true (typep '(0 0) 'nc:axial-position))
  (false (typep '(0) 'nc:axial-position))
  (false (typep '(0 0 0) 'nc:axial-position))
  (false (typep '(:a 0) 'nc:axial-position))
  (false (typep '(0 :a) 'nc:axial-position))
  (true (typep '(0 0 0) 'nc:cube-position))
  (false (typep '(0) 'nc:cube-position))
  (false (typep '(0 0) 'nc:cube-position))
  (false (typep '(:a 0 0) 'nc:cube-position))
  (false (typep '(0 :a 0) 'nc:cube-position))
  (false (typep '(0 0 :a) 'nc:cube-position))
  (false (typep '(0 0 1) 'nc:cube-position)))

(define-test coord-ensure-axial
  (let ((axial (nc:make-axial 0 0)))
    (is eqv axial (nc:ensure-axial (nc:make-axial 0 0)))
    (is eqv axial (nc:ensure-axial '(0 0)))
    (is eqv axial (nc:ensure-axial (nc:make-cube 0 0 0)))
    (is eqv axial (nc:ensure-axial '(0 0 0)))))

(define-test coord-ensure-cube
  (let ((cube (nc:make-cube 0 0 0)))
    (is eqv cube (nc:ensure-cube (nc:make-cube 0 0 0)))
    (is eqv cube (nc:ensure-cube '(0 0 0)))
    (is eqv cube (nc:ensure-cube (nc:make-axial 0 0)))
    (is eqv cube (nc:ensure-cube '(0 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations

(define-test coord-neighbors
  (let* ((neighbors (nc:neighbors *center-axial*))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-neighbors (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal neighbors expected-neighbors :test #'eqv))))

(define-test coord-diagonals
  (let* ((diagonals (nc:diagonals *center-axial*))
         (expected-axials '((2 -1) (1 1) (-1 2) (-2 1) (-1 -1) (1 -2)))
         (expected-diagonals (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal diagonals expected-diagonals :test #'eqv))))

(define-test coord-range
  (let* ((range (nc:range *center-axial* 1))
         (expected-axials '((0 0) (0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-range (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal range expected-range :test #'eqv))))

(define-test coord-distance
  (is = 0 (nc:distance *center-axial* *center-axial*))
  (is = 4 (nc:distance (nc:ensure-axial '(2 0)) (nc:ensure-axial '(-2 0))))
  (is = 4 (nc:distance (nc:ensure-axial '(-2 0)) (nc:ensure-axial '(2 0)))))

(define-test coord-range-intersection
  (let* ((axial-1 (nc:ensure-axial '(-2 0)))
         (axial-2 (nc:ensure-axial '(2 -2)))
         (intersection (nc:range-intersection axial-1 2 axial-2 2))
         (expected-axials '((0 -2) (0 -1) (0 0)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal intersection expected :test #'eqv))))

(define-test coord-rotate
  (let* ((axial (nc:ensure-axial '(0 0)))
         (center (nc:ensure-axial '(0 -1)))
         (expected (nc:ensure-axial '(-1 0)))
         (rotated (nc:rotate axial center 1)))
    (is eqv expected rotated)))

(define-test coord-ring
  (let* ((ring (nc:ring *center-axial* 1))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-ring (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal ring expected-ring :test #'eqv))))

(define-test coord-spiral-ring
  (let ((ring (nc:spiral-ring *center-axial* 0)))
    (is = 1 (length ring))
    (is eqv *center-axial* (first ring)))
  (let* ((ring (nc:spiral-ring *center-axial* 2))
         (expected-axials '((0 0)
                            (0 1) (-1 1) (-1 0) (0 -1) (1 -1) (1 0)
                            (0 2) (-1 2) (-2 2) (-2 1) (-2 0) (-1 -1)
                            (0 -2) (1 -2) (2 -2) (2 -1) (2 0) (1 1)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is = (length expected) (length ring))
    (true (every #'eqv ring expected))))

(define-test coord-linedraw
  (let* ((start (nc:ensure-axial '(-2 0)))
         (end (nc:ensure-axial '(2 -2)))
         (line (nc:linedraw start end))
         (expected-axials '((-2 0) (-1 0) (0 -1) (1 -1) (2 -2)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is = (length expected) (length line))
    (true (every #'eqv line expected))))
