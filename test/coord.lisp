;;;; test/board.lisp

(in-package #:nervous-island.test)

(define-test neighbors
  (let* ((neighbors (nb:neighbors nil *center*))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-neighbors (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal neighbors expected-neighbors :test #'equalp))))

(define-test neighbor
  (is equalp (nc:ensure-axial '(-1 0)) (nb:neighbor nil *center* :q))
  (is equalp (nc:ensure-axial '(0 -1)) (nb:neighbor nil *center* :w))
  (is equalp (nc:ensure-axial '(1 -1)) (nb:neighbor nil *center* :e))
  (is equalp (nc:ensure-axial '(-1 1)) (nb:neighbor nil *center* :a))
  (is equalp (nc:ensure-axial '(0 1)) (nb:neighbor nil *center* :s))
  (is equalp (nc:ensure-axial '(1 0)) (nb:neighbor nil *center* :d)))

(define-test diagonals
  (let* ((diagonals (nb:diagonals nil *center*))
         (expected-axials '((2 -1) (1 1) (-1 2) (-2 1) (-1 -1) (1 -2)))
         (expected-diagonals (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal diagonals expected-diagonals :test #'equalp))))

(define-test diagonal
  (is equalp (nc:ensure-axial '(-1 -1)) (nb:diagonal nil *center* :qw))
  (is equalp (nc:ensure-axial '(1 -2)) (nb:diagonal nil *center* :we))
  (is equalp (nc:ensure-axial '(2 -1)) (nb:diagonal nil *center* :ed))
  (is equalp (nc:ensure-axial '(1 1)) (nb:diagonal nil *center* :ds))
  (is equalp (nc:ensure-axial '(-1 2)) (nb:diagonal nil *center* :sa))
  (is equalp (nc:ensure-axial '(-2 1)) (nb:diagonal nil *center* :aq)))

(define-test range
  (let* ((range (nb:range nil *center* 1))
         (expected-axials '((0 0) (0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-range (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal range expected-range :test #'equalp))))

(define-test distance
  (is = 0 (nb:distance nil *center* *center*))
  (is = 4 (nb:distance nil (nc:ensure-axial '(2 0)) (nc:ensure-axial '(-2 0))))
  (is = 4 (nb:distance nil (nc:ensure-axial '(-2 0)) (nc:ensure-axial '(2 0)))))

(define-test range-intersection
  (let* ((axial-1 (nc:ensure-axial '(-2 0)))
         (axial-2 (nc:ensure-axial '(2 -2)))
         (intersection (nb:range-intersection nil axial-1 2 axial-2 2))
         (expected-axials '((0 -2) (0 -1) (0 0)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal intersection expected :test #'equalp))))

(define-test rotate
  (let* ((axial (nc:ensure-axial '(0 0)))
         (center (nc:ensure-axial '(0 -1)))
         (expected (nc:ensure-axial '(-1 0)))
         (rotated (nb:rotate nil axial center 1)))
    (is equalp expected rotated)))

(define-test ring
  (let* ((ring (nb:ring nil *center* 1))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-ring (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal ring expected-ring :test #'equalp))))

(define-test spiral-ring
  (let ((ring (nb:spiral-ring nil *center* 0)))
    (is = 1 (length ring))
    (is equalp *center* (first ring)))
  (let* ((ring (nb:spiral-ring nil *center* 2))
         (expected-axials '((0 0)
                            (0 1) (-1 1) (-1 0) (0 -1) (1 -1) (1 0)
                            (0 2) (-1 2) (-2 2) (-2 1) (-2 0) (-1 -1)
                            (0 -2) (1 -2) (2 -2) (2 -1) (2 0) (1 1)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is = (length expected) (length ring))
    (true (every #'equalp ring expected))))

(define-test linedraw
  (let* ((start (nc:ensure-axial '(-2 0)))
         (end (nc:ensure-axial '(2 -2)))
         (line (nc:linedraw start end))
         (expected-axials '((-2 0) (-1 0) (0 -1) (1 -1) (2 -2)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is = (length expected) (length line))
    (true (every #'equalp line expected))))

(define-test pathfind
  (let* ((start (nc:ensure-axial '(-2 0)))
         (end (nc:ensure-axial '(2 -2)))
         (path (nb:pathfind nil start end))
         (expected-axials '((-2 0) (-1 0) (0 -1) (1 -1) (2 -2)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is = (length expected) (length path))
    (true (every #'equalp path expected))))
