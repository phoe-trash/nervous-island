;;;; test/grid.lisp

(in-package #:nervous-island.test)

(define-test neighbors
  (let* ((neighbors (g:neighbors nil *center*))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-neighbors (mapcar #'g:ensure-space expected-axials)))
    (true (a:set-equal neighbors expected-neighbors :test #'g:space=))))

(define-test neighbors-board
  (let* ((board (g:make-board '(0 0) '(0 -1) '(1 -1) '(1 0)))
         (neighbors (g:neighbors board *center*))
         (expected-axials '((0 -1) (1 -1) (1 0)))
         (expected-neighbors (mapcar #'g:ensure-space expected-axials)))
    (true (a:set-equal neighbors expected-neighbors :test #'g:space=))))

(define-test neighbor
  (is g:space= (g:ensure-space '(-1 0)) (g:neighbor nil *center* :q))
  (is g:space= (g:ensure-space '(0 -1)) (g:neighbor nil *center* :w))
  (is g:space= (g:ensure-space '(1 -1)) (g:neighbor nil *center* :e))
  (is g:space= (g:ensure-space '(-1 1)) (g:neighbor nil *center* :a))
  (is g:space= (g:ensure-space '(0 1)) (g:neighbor nil *center* :s))
  (is g:space= (g:ensure-space '(1 0)) (g:neighbor nil *center* :d)))

(define-test neighbor-board
  (let* ((board (g:make-board '(0 0) '(0 -1) '(1 -1) '(1 0))))
    (is g:space= (g:ensure-space '(0 -1)) (g:neighbor board *center* :w))
    (is g:space= (g:ensure-space '(1 -1)) (g:neighbor board *center* :e))
    (is g:space= (g:ensure-space '(1 0)) (g:neighbor board *center* :d))
    (true (null (g:neighbor board *center* :q)))
    (true (null (g:neighbor board *center* :a)))
    (true (null (g:neighbor board *center* :s)))))

(define-test diagonals
  (let* ((diagonals (g:diagonals nil *center*))
         (expected-axials '((2 -1) (1 1) (-1 2) (-2 1) (-1 -1) (1 -2)))
         (expected-diagonals (mapcar #'g:ensure-space expected-axials)))
    (true (a:set-equal diagonals expected-diagonals :test #'g:space=))))

(define-test diagonals-board
  (let* ((board (g:make-board '(0 0) '(2 -1) '(1 1) '(-1 2)))
         (diagonals (g:diagonals board *center*))
         (expected-axials '((2 -1) (1 1) (-1 2)))
         (expected-diagonals (mapcar #'g:ensure-space expected-axials)))
    (true (a:set-equal diagonals expected-diagonals :test #'g:space=))))

(define-test diagonal
  (is g:space= (g:ensure-space '(-1 -1)) (g:diagonal nil *center* :qw))
  (is g:space= (g:ensure-space '(1 -2)) (g:diagonal nil *center* :we))
  (is g:space= (g:ensure-space '(2 -1)) (g:diagonal nil *center* :ed))
  (is g:space= (g:ensure-space '(1 1)) (g:diagonal nil *center* :ds))
  (is g:space= (g:ensure-space '(-1 2)) (g:diagonal nil *center* :sa))
  (is g:space= (g:ensure-space '(-2 1)) (g:diagonal nil *center* :aq)))

(define-test diagonal-board
  (let* ((board (g:make-board '(0 0) '(-1 -1) '(1 -2) '(2 -1))))
    (is g:space= (g:ensure-space '(-1 -1)) (g:diagonal board *center* :qw))
    (is g:space= (g:ensure-space '(1 -2)) (g:diagonal board *center* :we))
    (is g:space= (g:ensure-space '(2 -1)) (g:diagonal board *center* :ed))
    (true (null (g:diagonal board *center* :qa)))
    (true (null (g:diagonal board *center* :sd)))
    (true (null (g:diagonal board *center* :aq)))))

(define-test range
  (let* ((range (g:range nil *center* 1))
         (expected-axials '((0 0) (0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-range (mapcar #'g:ensure-space expected-axials)))
    (true (a:set-equal range expected-range :test #'g:space=))))

(define-test range-board
  (let* ((board (g:make-board '(0 0) '(0 1) '(0 2) '(0 3)))
         (range (g:range board *center* 2))
         (expected-axials '((0 0) (0 1) (0 2)))
         (expected-range (mapcar #'g:ensure-space expected-axials)))
    (true (a:set-equal range expected-range :test #'g:space=))))

(define-test distance
  (is = 0 (g:distance nil *center* *center*))
  (is = 4 (g:distance nil (g:ensure-space '(2 0)) (g:ensure-space '(-2 0))))
  (is = 4 (g:distance nil (g:ensure-space '(-2 0)) (g:ensure-space '(2 0)))))

(define-test distance-board
  (let* ((board (g:make-board '(0 -1) '(1 -1) '(1 0) '(0 1)))
         (start (g:ensure-space '(0 -1)))
         (end (g:ensure-space '(0 1))))
    (is = 3 (g:distance board start end))))

(define-test range-intersection
  (let* ((space-1 (g:ensure-space '(-2 0)))
         (space-2 (g:ensure-space '(2 -2)))
         (intersection (g:range-intersection nil space-1 2 space-2 2))
         (expected-axials '((0 -2) (0 -1) (0 0)))
         (expected (mapcar #'g:ensure-space expected-axials)))
    (true (a:set-equal intersection expected :test #'g:space=))))

(define-test range-intersection-board
  (let* ((board (make-standard-board))
         (space-1 (g:ensure-space '(-2 0)))
         (space-2 (g:ensure-space '(-1 -1)))
         (intersection (g:range-intersection board space-1 1 space-2 1))
         (expected-axials '((-2 0) (-1 -1) (-1 0)))
         (expected (mapcar #'g:ensure-space expected-axials)))
    (true (a:set-equal intersection expected :test #'g:space=))))

(define-test rotate
  (let* ((space (g:ensure-space '(0 0)))
         (center (g:ensure-space '(0 -1)))
         (expected (g:ensure-space '(-1 0)))
         (rotated (g:rotate nil space center 1)))
    (is g:space= expected rotated)))

(define-test rotate-board
  (let* ((board (g:make-board '(0 0) '(0 -1)))
         (space (g:ensure-space '(0 0)))
         (center (g:ensure-space '(0 -1)))
         (rotated (g:rotate board space center 1)))
    (true (null rotated))))

(define-test ring
  (let* ((ring (g:ring nil *center* 1))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1) (-1 1) (-1 0)))
         (expected-ring (mapcar #'g:ensure-space expected-axials)))
    (true (a:set-equal ring expected-ring :test #'g:space=))))

(define-test ring-board
  (let* ((board (g:make-board '(0 0) '(0 -1) '(1 -1) '(1 0)))
         (ring (g:ring board *center* 1))
         (expected-axials '((0 -1) (1 -1) (1 0)))
         (expected-ring (mapcar #'g:ensure-space expected-axials)))
    (true (a:set-equal ring expected-ring :test #'g:space=))))

(define-test spiral-ring
  (let ((ring (g:spiral-ring nil *center* 0)))
    (is = 1 (length ring))
    (is g:space= *center* (first ring)))
  (let* ((ring (g:spiral-ring nil *center* 2))
         (expected-axials '((0 0)
                            (0 1) (-1 1) (-1 0) (0 -1) (1 -1) (1 0)
                            (0 2) (-1 2) (-2 2) (-2 1) (-2 0) (-1 -1)
                            (0 -2) (1 -2) (2 -2) (2 -1) (2 0) (1 1)))
         (expected (mapcar #'g:ensure-space expected-axials)))
    (is = (length expected) (length ring))
    (true (every #'g:space= ring expected))))

(define-test spiral-ring-board
  (let* ((board (g:make-board '(0 0) '(0 -1) '(1 -1) '(1 0) '(0 -2)))
         (ring (g:spiral-ring board *center* 2))
         (expected-axials '((0 0) (0 -1) (1 -1) (1 0) (0 -2)))
         (expected (mapcar #'g:ensure-space expected-axials)))
    (is = (length expected) (length ring))
    (true (every #'g:space= ring expected))))

(define-test linedraw
  (let* ((start (g:ensure-space '(-2 0)))
         (end (g:ensure-space '(2 -2)))
         (line (g:linedraw start end))
         (expected-axials '((-2 0) (-1 0) (0 -1) (1 -1) (2 -2)))
         (expected (mapcar #'g:ensure-space expected-axials)))
    (is = (length expected) (length line))
    (true (every #'g:space= line expected))))

(define-test pathfind
  (let* ((start (g:ensure-space '(-2 0)))
         (end (g:ensure-space '(2 -2)))
         (path (g:pathfind nil start end))
         (expected-axials '((-2 0) (-1 0) (0 -1) (1 -1) (2 -2)))
         (expected (mapcar #'g:ensure-space expected-axials)))
    (is = (length expected) (length path))
    (true (every #'g:space= path expected))))

(define-test pathfind-board
  (let* ((board (g:make-board '(0 -1) '(1 -1) '(1 0) '(0 1)))
         (start (g:ensure-space '(0 -1)))
         (end (g:ensure-space '(0 1)))
         (path (g:pathfind board start end))
         (expected-axials '((0 -1) (1 -1) (1 0) (0 1)))
         (expected (mapcar #'g:ensure-space expected-axials)))
    (is = (length expected) (length path))
    (true (every #'g:space= path expected))))
