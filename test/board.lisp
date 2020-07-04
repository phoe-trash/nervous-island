;;;; test/board.lisp

(in-package #:nervous-island.test)

(define-test neighbors-board
  (let* ((board (nb:make-board '(0 0) '(0 -1) '(1 -1) '(1 0)))
         (neighbors (nb:neighbors board *center*))
         (expected-axials '((0 -1) (1 -1) (1 0)))
         (expected-neighbors (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal neighbors expected-neighbors :test #'equalp))))

(define-test neighbor-board
  (let* ((board (nb:make-board '(0 0) '(0 -1) '(1 -1) '(1 0))))
    (is equalp (nc:ensure-axial '(0 -1)) (nb:neighbor board *center* :w))
    (is equalp (nc:ensure-axial '(1 -1)) (nb:neighbor board *center* :e))
    (is equalp (nc:ensure-axial '(1 0)) (nb:neighbor board *center* :d))
    (true (null (nb:neighbor board *center* :q)))
    (true (null (nb:neighbor board *center* :a)))
    (true (null (nb:neighbor board *center* :s)))))

(define-test diagonals-board
  (let* ((board (nb:make-board '(0 0) '(2 -1) '(1 1) '(-1 2)))
         (diagonals (nb:diagonals board *center*))
         (expected-axials '((2 -1) (1 1) (-1 2)))
         (expected-diagonals (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal diagonals expected-diagonals :test #'equalp))))

(define-test diagonal-board
  (let* ((board (nb:make-board '(0 0) '(-1 -1) '(1 -2) '(2 -1))))
    (is equalp (nc:ensure-axial '(-1 -1)) (nb:diagonal board *center* :qw))
    (is equalp (nc:ensure-axial '(1 -2)) (nb:diagonal board *center* :we))
    (is equalp (nc:ensure-axial '(2 -1)) (nb:diagonal board *center* :ed))
    (true (null (nb:diagonal board *center* :qa)))
    (true (null (nb:diagonal board *center* :sd)))
    (true (null (nb:diagonal board *center* :aq)))))

(define-test range-board
  (let* ((board (nb:make-board '(0 0) '(0 1) '(0 2) '(0 3)))
         (range (nb:range board *center* 2))
         (expected-axials '((0 0) (0 1) (0 2)))
         (expected-range (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal range expected-range :test #'equalp))))

(define-test distance-board
  (let* ((board (nb:make-board '(0 -1) '(1 -1) '(1 0) '(0 1)))
         (start (nc:ensure-axial '(0 -1)))
         (end (nc:ensure-axial '(0 1))))
    (is = 3 (nb:distance board start end))))

(define-test range-intersection-board
  (let* ((board (make-standard-board))
         (axial-1 (nc:ensure-axial '(-2 0)))
         (axial-2 (nc:ensure-axial '(-1 -1)))
         (intersection (nb:range-intersection board axial-1 1 axial-2 1))
         (expected-axials '((-2 0) (-1 -1) (-1 0)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal intersection expected :test #'equalp))))

(define-test rotate-board
  (let* ((board (nb:make-board '(0 0) '(0 -1)))
         (axial (nc:ensure-axial '(0 0)))
         (center (nc:ensure-axial '(0 -1)))
         (rotated (nb:rotate board axial center 1)))
    (true (null rotated))))

(define-test ring-board
  (let* ((board (nb:make-board '(0 0) '(0 -1) '(1 -1) '(1 0)))
         (ring (nb:ring board *center* 1))
         (expected-axials '((0 -1) (1 -1) (1 0)))
         (expected-ring (mapcar #'nc:ensure-axial expected-axials)))
    (true (a:set-equal ring expected-ring :test #'equalp))))

(define-test spiral-ring-board
  (let* ((board (nb:make-board '(0 0) '(0 -1) '(1 -1) '(1 0) '(0 -2)))
         (ring (nb:spiral-ring board *center* 2))
         (expected-axials '((0 0) (0 -1) (1 -1) (1 0) (0 -2)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is = (length expected) (length ring))
    (true (every #'equalp ring expected))))

(define-test pathfind-board
  (let* ((board (nb:make-board '(0 -1) '(1 -1) '(1 0) '(0 1)))
         (start (nc:ensure-axial '(0 -1)))
         (end (nc:ensure-axial '(0 1)))
         (path (nb:pathfind board start end))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1)))
         (expected (mapcar #'nc:ensure-axial expected-axials)))
    (is = (length expected) (length path))
    (true (every #'equalp path expected))))
