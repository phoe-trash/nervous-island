;;;; test/state/board.lisp

(in-package #:nervous-island.test)

(define-test board-instantiation
  (let ((board (nb:board)))
    (is = 0 (dict-count (nb:spaces board))))
  (let ((board (nb:board '(0 0) '(0 0))))
    (is = 1 (dict-count (nb:spaces board)))
    (is eqv (nsp:space '(0 0)) (dict-find (nb:spaces board) (nc:axial 0 0))))
  (let* ((board (nb:board '(0 0) '(0 -1) '(0 1)))
         (spaces (nb:spaces board)))
    (is = 3 (dict-count spaces))
    (is eqv (nsp:space '(0 0)) (dict-find spaces (nc:axial 0 0)))
    (is eqv (nsp:space '(0 -1)) (dict-find spaces (nc:axial 0 -1)))
    (is eqv (nsp:space '(0 1)) (dict-find spaces (nc:axial 0 1)))
    (false (dict-find spaces (nc:axial 1 -1)))
    (true (nb:find-space board (nc:axial 0 0)))
    (true (nb:find-space board (nc:axial 0 -1)))
    (true (nb:find-space board (nc:axial 0 1)))
    (false (nb:find-space board (nc:axial 1 -1)))
    (let* ((actual-axials '((0 0) (0 -1) (0 1) (1 -1)))
           (actual (apply #'set (mapcar #'nc:ensure-axial actual-axials)))
           (expected-axials '((0 0) (0 -1) (0 1)))
           (expected (apply #'set (mapcar #'nc:ensure-axial expected-axials))))
      (is eqv (nb:remove-missing-axials board actual) expected))))

(define-test board-neighbors
  (let* ((board (nb:board '(0 0) '(0 -1) '(1 -1) '(1 0)))
         (neighbors (nb:neighbors board *center-axial*))
         (expected-axials '((0 -1) (1 -1) (1 0)))
         (expected (apply #'set (mapcar #'nc:ensure-axial expected-axials))))
    (is eqv neighbors expected)))

(define-test board-neighbor
  (let* ((board (nb:board '(0 0) '(0 -1) '(1 -1) '(1 0))))
    (is eqv (nc:ensure-axial '(0 -1)) (nb:neighbor board *center-axial* :w))
    (is eqv (nc:ensure-axial '(1 -1)) (nb:neighbor board *center-axial* :e))
    (is eqv (nc:ensure-axial '(1 0)) (nb:neighbor board *center-axial* :d))
    (true (null (nb:neighbor board *center-axial* :q)))
    (true (null (nb:neighbor board *center-axial* :a)))
    (true (null (nb:neighbor board *center-axial* :s)))))

(define-test board-diagonals
  (let* ((board (nb:board '(0 0) '(2 -1) '(1 1) '(-1 2)))
         (diagonals (nb:diagonals board *center-axial*))
         (expected-axials '((2 -1) (1 1) (-1 2)))
         (expected (apply #'set (mapcar #'nc:ensure-axial expected-axials))))
    (is eqv diagonals expected)))

(define-test board-diagonal
  (let* ((board (nb:board '(0 0) '(-1 -1) '(1 -2) '(2 -1))))
    (is eqv (nc:ensure-axial '(-1 -1)) (nb:diagonal board *center-axial* :qw))
    (is eqv (nc:ensure-axial '(1 -2)) (nb:diagonal board *center-axial* :we))
    (is eqv (nc:ensure-axial '(2 -1)) (nb:diagonal board *center-axial* :ed))
    (true (null (nb:diagonal board *center-axial* :ds)))
    (true (null (nb:diagonal board *center-axial* :sa)))
    (true (null (nb:diagonal board *center-axial* :aq)))))

(define-test board-range
  (let* ((board (nb:board '(0 0) '(0 1) '(0 2) '(0 3)))
         (range (nb:range board *center-axial* 2))
         (expected-axials '((0 0) (0 1) (0 2)))
         (expected (apply #'set (mapcar #'nc:ensure-axial expected-axials))))
    (is eqv range expected)))

(define-test board-range-intersection
  (let* ((board (nb:standard-board))
         (axial-1 (nc:ensure-axial '(-2 0)))
         (axial-2 (nc:ensure-axial '(-1 -1)))
         (intersection (nb:range-intersection board axial-1 1 axial-2 1))
         (expected-axials '((-2 0) (-1 -1) (-1 0)))
         (expected (apply #'set (mapcar #'nc:ensure-axial expected-axials))))
    (is eqv intersection expected)))

(define-test board-rotate
  (let* ((board (nb:board '(0 0) '(0 -1)))
         (axial (nc:ensure-axial '(0 0)))
         (center (nc:ensure-axial '(0 -1)))
         (rotated (nb:rotate board axial center 1)))
    (true (null rotated))))

(define-test board-ring
  (let* ((board (nb:board '(0 0) '(0 -1) '(1 -1) '(1 0)))
         (ring (nb:ring board *center-axial* 1))
         (expected-axials '((0 -1) (1 -1) (1 0)))
         (expected (apply #'set (mapcar #'nc:ensure-axial expected-axials))))
    (is eqv ring expected)))

(define-test board-spiral
  (let* ((board (nb:board '(0 0) '(0 -1) '(1 -1) '(1 0) '(0 -2)))
         (ring (nb:spiral board *center-axial* 2))
         (expected-axials '((0 0) (0 -1) (1 -1) (1 0) (0 -2)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is = (length expected) (length ring))
    (true (every #'eqv ring expected))))

(define-test board-distance
  (let* ((board (nb:board '(0 -1) '(1 -1) '(1 0) '(0 1)))
         (start (nc:ensure-axial '(0 -1)))
         (end (nc:ensure-axial '(0 1))))
    (is = 3 (nb:distance board start end))))

(define-test board-pathfind
  (let* ((board (nb:board '(0 -1) '(1 -1) '(1 0) '(0 1)))
         (start (nc:ensure-axial '(0 -1)))
         (end (nc:ensure-axial '(0 1)))
         (path (nb:pathfind board start end))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is eqv path expected)))
