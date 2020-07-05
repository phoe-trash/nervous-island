;;;; test/space.lisp

(in-package #:nervous-island.test)

(defclass space-test-tile (nt:tile) ())

(defclass space-test-foundation (nt:foundation) ())

;; TODO test REINITIALIZE-INSTANCE everywhere

(define-test space-instantiation
  (let ((axial (nc:make-axial 0 0))
        (tile (make-instance 'space-test-tile))
        (foundation (make-instance 'space-test-foundation)))
    (fail (make-instance 'nsp:space))
    (fail (make-instance 'nsp:space :axial 42) 'type-error)
    (fail (make-instance 'nsp:space :axial axial :tokens 42) 'type-error)
    (fail (make-instance 'nsp:space :axial axial :tile 42) 'type-error)
    (fail (make-instance 'nsp:space :axial axial :tile tile))
    (fail (make-instance 'nsp:space :axial axial :rotation :w))
    (fail (make-instance 'nsp:space :axial axial :tile tile :rotation 42)
        'type-error)
    (fail (make-instance 'nsp:space :axial axial :tile foundation :rotation :w)
        'type-error)
    (fail (make-instance 'nsp:space :axial axial :foundation tile)
        'type-error)
    (true (make-instance 'nsp:space :axial axial :tile tile :rotation :w))
    (true (make-instance 'nsp:space :axial axial :tile tile :rotation :w
                                    :foundation foundation))
    (true (make-instance 'nsp:space :axial axial :foundation foundation))))

(define-test space-reinitialize
  (let* ((axial (nc:make-axial 0 0))
         (tile (make-instance 'space-test-tile))
         (rotation :w)
         (foundation (make-instance 'space-test-foundation))
         (space (make-instance 'nsp:space :axial axial :rotation rotation
                                          :tile tile :foundation foundation)))
    (is eq axial (nsp:axial space))
    (is eq tile (nsp:tile space))
    (is eq rotation (nsp:rotation space))
    (is eq foundation (nsp:foundation space))
    (is eq space (reinitialize-instance space))
    ;; TODO test tokens here
    (is eq axial (nsp:axial space))
    (is eq tile (nsp:tile space))
    (is eq rotation (nsp:rotation space))
    (is eq foundation (nsp:foundation space))))

(define-test space-make-spaces
  (let* ((things (list (nc:make-axial 0 0)
                       (make-instance 'nsp:space :axial (nc:make-axial 0 1))))
         (axials (mapcar #'nc:ensure-axial '((0 0) (0 1))))
         (spaces (nsp:make-spaces things)))
    (true (typep spaces 'hash-table))
    (is = 2 (hash-table-count spaces))
    (dolist (axial axials)
      (let ((thing (gethash axial spaces)))
        (true (typep thing 'nsp:space))
        (is eq '() (nsp:tokens thing))
        (is eq nil (nsp:tile thing))
        (is eq nil (nsp:rotation thing))
        (is eq nil (nsp:foundation thing))))))

(define-test space-edit-space
  (let* ((axial (nc:make-axial 0 0))
         (tile-1 (make-instance 'space-test-tile))
         (tile-2 (make-instance 'space-test-tile))
         (foundation-1 (make-instance 'space-test-foundation))
         (foundation-2 (make-instance 'space-test-foundation))
         (space-1 (make-instance 'nsp:space :axial axial
                                            :tile tile-1 :rotation :w
                                            :foundation foundation-1))
         (space-2 (nsp:edit-space space-1 :tile tile-2 :rotation :s
                                          :foundation foundation-2)))
    ;; TODO test tokens here
    ;; TODO test axials here
    (isnt eq space-1 space-2)
    (is equalp (nsp:axial space-1) (nsp:axial space-2))
    (is eq tile-1 (nsp:tile space-1))
    (is eq :w (nsp:rotation space-1))
    (is eq foundation-1 (nsp:foundation space-1))
    (is eq tile-2 (nsp:tile space-2))
    (is eq :s (nsp:rotation space-2))
    (is eq foundation-2 (nsp:foundation space-2))))

(define-test space-edit-spaces
  (let ((space-1 (make-instance 'nsp:space))
        (space-2 (make-instance 'nsp:space)))
    ;; TODO
    ))

(define-test space-find-tile)
