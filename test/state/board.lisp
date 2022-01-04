;;;; test/state/board.lisp

(in-package #:nervous-island.test)

(define-test board-instantiation
  (let ((board (nb:make-board)))
    (is = 0 (hash-table-count (nb:axials board))))
  (let* ((board (nb:make-board '(0 0) '(0 -1) '(0 1)))
         (axials (nb:axials board)))
    (is = 3 (hash-table-count axials))
    (is equalp (nc:make-axial 0 0) (gethash (nc:make-axial 0 0) axials))
    (is equalp (nc:make-axial 0 -1) (gethash (nc:make-axial 0 -1) axials))
    (is equalp (nc:make-axial 0 1) (gethash (nc:make-axial 0 1) axials))
    (is equalp nil (gethash (nc:make-axial 1 -1) axials))
    (true (nb:axial-present-p board (nc:make-axial 0 0)))
    (true (nb:axial-present-p board (nc:make-axial 0 -1)))
    (true (nb:axial-present-p board (nc:make-axial 0 1)))
    (false (nb:axial-present-p board (nc:make-axial 1 -1)))
    (let* ((actual-axials '((0 0) (0 -1) (0 1) (1 -1)))
           (actual (mapcar #'nc:ensure-axial actual-axials))
           (expected-axials '((0 0) (0 -1) (0 1)))
           (expected (mapcar #'nc:ensure-axial expected-axials)))
      (true (a:set-equal (nb:only-present-axials board actual) expected
                         :test #'equalp))))
  (fail (nb:make-board '(0 0) '(0 0)) 'nb:duplicated-axial))

(define-test board-neighbors
  (let* ((board (nb:make-board '(0 0) '(0 -1) '(1 -1) '(1 0)))
         (neighbors (nb:neighbors board *center-axial*))
         (expected-axials '((0 -1) (1 -1) (1 0)))
         (expected-neighbors (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal neighbors expected-neighbors :test #'equalp))))

(define-test board-neighbor
  (let* ((board (nb:make-board '(0 0) '(0 -1) '(1 -1) '(1 0))))
    (is equalp (nc:ensure-axial '(0 -1)) (nb:neighbor board *center-axial* :w))
    (is equalp (nc:ensure-axial '(1 -1)) (nb:neighbor board *center-axial* :e))
    (is equalp (nc:ensure-axial '(1 0)) (nb:neighbor board *center-axial* :d))
    (true (null (nb:neighbor board *center-axial* :q)))
    (true (null (nb:neighbor board *center-axial* :a)))
    (true (null (nb:neighbor board *center-axial* :s)))))

(define-test board-diagonals
  (let* ((board (nb:make-board '(0 0) '(2 -1) '(1 1) '(-1 2)))
         (diagonals (nb:diagonals board *center-axial*))
         (expected-axials '((2 -1) (1 1) (-1 2)))
         (expected-diagonals (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal diagonals expected-diagonals :test #'equalp))))

(define-test board-diagonal
  (let* ((board (nb:make-board '(0 0) '(-1 -1) '(1 -2) '(2 -1))))
    (is equalp (nc:ensure-axial '(-1 -1)) (nb:diagonal board *center-axial* :qw))
    (is equalp (nc:ensure-axial '(1 -2)) (nb:diagonal board *center-axial* :we))
    (is equalp (nc:ensure-axial '(2 -1)) (nb:diagonal board *center-axial* :ed))
    (true (null (nb:diagonal board *center-axial* :ds)))
    (true (null (nb:diagonal board *center-axial* :sa)))
    (true (null (nb:diagonal board *center-axial* :aq)))))

(define-test board-range
  (let* ((board (nb:make-board '(0 0) '(0 1) '(0 2) '(0 3)))
         (range (nb:range board *center-axial* 2))
         (expected-axials '((0 0) (0 1) (0 2)))
         (expected-range (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal range expected-range :test #'equalp))))

(define-test board-range-intersection
  (let* ((board (make-standard-board))
         (axial-1 (nc:ensure-axial '(-2 0)))
         (axial-2 (nc:ensure-axial '(-1 -1)))
         (intersection (nb:range-intersection board axial-1 1 axial-2 1))
         (expected-axials '((-2 0) (-1 -1) (-1 0)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal intersection expected :test #'equalp))))

(define-test board-rotate
  (let* ((board (nb:make-board '(0 0) '(0 -1)))
         (axial (nc:ensure-axial '(0 0)))
         (center (nc:ensure-axial '(0 -1)))
         (rotated (nb:rotate board axial center 1)))
    (true (null rotated))))

(define-test board-ring
  (let* ((board (nb:make-board '(0 0) '(0 -1) '(1 -1) '(1 0)))
         (ring (nb:ring board *center-axial* 1))
         (expected-axials '((0 -1) (1 -1) (1 0)))
         (expected-ring (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal ring expected-ring :test #'equalp))))

(define-test board-spiral-ring
  (let* ((board (nb:make-board '(0 0) '(0 -1) '(1 -1) '(1 0) '(0 -2)))
         (ring (nb:spiral-ring board *center-axial* 2))
         (expected-axials '((0 0) (0 -1) (1 -1) (1 0) (0 -2)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is = (length expected) (length ring))
    (true (every #'equalp ring expected))))

(define-test board-distance
  (let* ((board (nb:make-board '(0 -1) '(1 -1) '(1 0) '(0 1)))
         (start (nc:ensure-axial '(0 -1)))
         (end (nc:ensure-axial '(0 1))))
    (is = 3 (nb:distance board start end))))

(define-test board-pathfind
  (let* ((board (nb:make-board '(0 -1) '(1 -1) '(1 0) '(0 1)))
         (start (nc:ensure-axial '(0 -1)))
         (end (nc:ensure-axial '(0 1)))
         (path (nb:pathfind board start end))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is = (length expected) (length path))
    (true (every #'equalp path expected))))
