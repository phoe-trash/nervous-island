;;;; test/grid.lisp

(in-package #:nervous-island.test)

(define-test neighbors
  (let* ((neighbors (g:neighbors nil *center*))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-neighbors (mapcar #'g:ensure-axial expected-axials)))
    (true (a:set-equal neighbors expected-neighbors :test #'equalp))))

(define-test neighbors-board
  (let* ((board (g:make-board '(0 0) '(0 -1) '(1 -1) '(1 0)))
         (neighbors (g:neighbors board *center*))
         (expected-axials '((0 -1) (1 -1) (1 0)))
         (expected-neighbors (mapcar #'g:ensure-axial expected-axials)))
    (true (a:set-equal neighbors expected-neighbors :test #'equalp))))

(define-test neighbor
  (is equalp (g:ensure-axial '(-1 0)) (g:neighbor nil *center* :q))
  (is equalp (g:ensure-axial '(0 -1)) (g:neighbor nil *center* :w))
  (is equalp (g:ensure-axial '(1 -1)) (g:neighbor nil *center* :e))
  (is equalp (g:ensure-axial '(-1 1)) (g:neighbor nil *center* :a))
  (is equalp (g:ensure-axial '(0 1)) (g:neighbor nil *center* :s))
  (is equalp (g:ensure-axial '(1 0)) (g:neighbor nil *center* :d)))

(define-test neighbor-board
  (let* ((board (g:make-board '(0 0) '(0 -1) '(1 -1) '(1 0))))
    (is equalp (g:ensure-axial '(0 -1)) (g:neighbor board *center* :w))
    (is equalp (g:ensure-axial '(1 -1)) (g:neighbor board *center* :e))
    (is equalp (g:ensure-axial '(1 0)) (g:neighbor board *center* :d))
    (true (null (g:neighbor board *center* :q)))
    (true (null (g:neighbor board *center* :a)))
    (true (null (g:neighbor board *center* :s)))))

(define-test diagonals
  (let* ((diagonals (g:diagonals nil *center*))
         (expected-axials '((2 -1) (1 1) (-1 2) (-2 1) (-1 -1) (1 -2)))
         (expected-diagonals (mapcar #'g:ensure-axial expected-axials)))
    (true (a:set-equal diagonals expected-diagonals :test #'equalp))))

(define-test diagonals-board
  (let* ((board (g:make-board '(0 0) '(2 -1) '(1 1) '(-1 2)))
         (diagonals (g:diagonals board *center*))
         (expected-axials '((2 -1) (1 1) (-1 2)))
         (expected-diagonals (mapcar #'g:ensure-axial expected-axials)))
    (true (a:set-equal diagonals expected-diagonals :test #'equalp))))

(define-test diagonal
  (is equalp (g:ensure-axial '(-1 -1)) (g:diagonal nil *center* :qw))
  (is equalp (g:ensure-axial '(1 -2)) (g:diagonal nil *center* :we))
  (is equalp (g:ensure-axial '(2 -1)) (g:diagonal nil *center* :ed))
  (is equalp (g:ensure-axial '(1 1)) (g:diagonal nil *center* :ds))
  (is equalp (g:ensure-axial '(-1 2)) (g:diagonal nil *center* :sa))
  (is equalp (g:ensure-axial '(-2 1)) (g:diagonal nil *center* :aq)))

(define-test diagonal-board
  (let* ((board (g:make-board '(0 0) '(-1 -1) '(1 -2) '(2 -1))))
    (is equalp (g:ensure-axial '(-1 -1)) (g:diagonal board *center* :qw))
    (is equalp (g:ensure-axial '(1 -2)) (g:diagonal board *center* :we))
    (is equalp (g:ensure-axial '(2 -1)) (g:diagonal board *center* :ed))
    (true (null (g:diagonal board *center* :qa)))
    (true (null (g:diagonal board *center* :sd)))
    (true (null (g:diagonal board *center* :aq)))))

(define-test range
  (let* ((range (g:range nil *center* 1))
         (expected-axials '((0 0) (0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-range (mapcar #'g:ensure-axial expected-axials)))
    (true (a:set-equal range expected-range :test #'equalp))))

(define-test range-board
  (let* ((board (g:make-board '(0 0) '(0 1) '(0 2) '(0 3)))
         (range (g:range board *center* 2))
         (expected-axials '((0 0) (0 1) (0 2)))
         (expected-range (mapcar #'g:ensure-axial expected-axials)))
    (true (a:set-equal range expected-range :test #'equalp))))

(define-test distance
  (is = 0 (g:distance nil *center* *center*))
  (is = 4 (g:distance nil (g:ensure-axial '(2 0)) (g:ensure-axial '(-2 0))))
  (is = 4 (g:distance nil (g:ensure-axial '(-2 0)) (g:ensure-axial '(2 0)))))

(define-test distance-board
  (let* ((board (g:make-board '(0 -1) '(1 -1) '(1 0) '(0 1)))
         (start (g:ensure-axial '(0 -1)))
         (end (g:ensure-axial '(0 1))))
    (is = 3 (g:distance board start end))))

(define-test range-intersection
  (let* ((axial-1 (g:ensure-axial '(-2 0)))
         (axial-2 (g:ensure-axial '(2 -2)))
         (intersection (g:range-intersection nil axial-1 2 axial-2 2))
         (expected-axials '((0 -2) (0 -1) (0 0)))
         (expected (mapcar #'g:ensure-axial expected-axials)))
    (true (a:set-equal intersection expected :test #'equalp))))

(define-test range-intersection-board
  (let* ((board (make-standard-board))
         (axial-1 (g:ensure-axial '(-2 0)))
         (axial-2 (g:ensure-axial '(-1 -1)))
         (intersection (g:range-intersection board axial-1 1 axial-2 1))
         (expected-axials '((-2 0) (-1 -1) (-1 0)))
         (expected (mapcar #'g:ensure-axial expected-axials)))
    (true (a:set-equal intersection expected :test #'equalp))))

(define-test rotate
  (let* ((axial (g:ensure-axial '(0 0)))
         (center (g:ensure-axial '(0 -1)))
         (expected (g:ensure-axial '(-1 0)))
         (rotated (g:rotate nil axial center 1)))
    (is equalp expected rotated)))

(define-test rotate-board
  (let* ((board (g:make-board '(0 0) '(0 -1)))
         (axial (g:ensure-axial '(0 0)))
         (center (g:ensure-axial '(0 -1)))
         (rotated (g:rotate board axial center 1)))
    (true (null rotated))))

(define-test ring
  (let* ((ring (g:ring nil *center* 1))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-ring (mapcar #'g:ensure-axial expected-axials)))
    (true (a:set-equal ring expected-ring :test #'equalp))))

(define-test ring-board
  (let* ((board (g:make-board '(0 0) '(0 -1) '(1 -1) '(1 0)))
         (ring (g:ring board *center* 1))
         (expected-axials '((0 -1) (1 -1) (1 0)))
         (expected-ring (mapcar #'g:ensure-axial expected-axials)))
    (true (a:set-equal ring expected-ring :test #'equalp))))

(define-test spiral-ring
  (let ((ring (g:spiral-ring nil *center* 0)))
    (is = 1 (length ring))
    (is equalp *center* (first ring)))
  (let* ((ring (g:spiral-ring nil *center* 2))
         (expected-axials '((0 0)
                            (0 1) (-1 1) (-1 0) (0 -1) (1 -1) (1 0)
                            (0 2) (-1 2) (-2 2) (-2 1) (-2 0) (-1 -1)
                            (0 -2) (1 -2) (2 -2) (2 -1) (2 0) (1 1)))
         (expected (mapcar #'g:ensure-axial expected-axials)))
    (is = (length expected) (length ring))
    (true (every #'equalp ring expected))))

(define-test spiral-ring-board
  (let* ((board (g:make-board '(0 0) '(0 -1) '(1 -1) '(1 0) '(0 -2)))
         (ring (g:spiral-ring board *center* 2))
         (expected-axials '((0 0) (0 -1) (1 -1) (1 0) (0 -2)))
         (expected (mapcar #'g:ensure-axial expected-axials)))
    (is = (length expected) (length ring))
    (true (every #'equalp ring expected))))

(define-test linedraw
  (let* ((start (g:ensure-axial '(-2 0)))
         (end (g:ensure-axial '(2 -2)))
         (line (g:linedraw start end))
         (expected-axials '((-2 0) (-1 0) (0 -1) (1 -1) (2 -2)))
         (expected (mapcar #'g:ensure-axial expected-axials)))
    (is = (length expected) (length line))
    (true (every #'equalp line expected))))

(define-test pathfind
  (let* ((start (g:ensure-axial '(-2 0)))
         (end (g:ensure-axial '(2 -2)))
         (path (g:pathfind nil start end))
         (expected-axials '((-2 0) (-1 0) (0 -1) (1 -1) (2 -2)))
         (expected (mapcar #'g:ensure-axial expected-axials)))
    (is = (length expected) (length path))
    (true (every #'equalp path expected))))

(define-test pathfind-board
  (let* ((board (g:make-board '(0 -1) '(1 -1) '(1 0) '(0 1)))
         (start (g:ensure-axial '(0 -1)))
         (end (g:ensure-axial '(0 1)))
         (path (g:pathfind board start end))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1)))
         (expected (mapcar #'g:ensure-axial expected-axials)))
    (is = (length expected) (length path))
    (true (every #'equalp path expected))))
