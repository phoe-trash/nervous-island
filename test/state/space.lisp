;;;; test/state/space.lisp

(in-package #:nervous-island.test)

(define-class space-test-instant (nt:instant) ())
(define-class space-test-unit (nt:unit) ())
(define-class space-test-foundation (nt:foundation) ())
(define-class space-test-token (nto:token) ())

(define-test space-instantiation
  (let ((axial (nc:axial 0 0))
        (tokens (list (make-instance 'space-test-token)))
        (unit (make-instance 'space-test-unit))
        (unit-rotation 3)
        (foundation (make-instance 'space-test-foundation)))
    (flet ((make (&rest args) (apply #'make-instance 'nsp:space args)))
      (fail (make))
      (fail (make :axial 42) 'type-error)
      (fail (make :axial axial :tokens 42) 'type-error)
      (fail (make :axial axial :unit 42 :unit-rotation unit-rotation)
          'type-error)
      (fail (make :axial axial :unit unit))
      (fail (make :axial axial :unit-rotation unit-rotation))
      (fail (make :axial axial :unit unit :unit-rotation :w) 'type-error)
      (fail (make :axial axial :unit foundation :unit-rotation unit-rotation)
          'type-error)
      (fail (make :axial axial :foundation unit) 'type-error)
      (true (make :axial axial :unit unit :unit-rotation unit-rotation))
      (true (make :axial axial :unit unit :unit-rotation unit-rotation
                  :foundation foundation :tokens tokens))
      (true (make :axial axial :foundation foundation))
      (true (make :axial axial :tokens tokens)))))

(define-test space-reinitialize
  (let* ((axial (nc:axial 0 0))
         (tokens (list (make-instance 'space-test-token)))
         (overlay (make-instance 'space-test-instant))
         (unit (make-instance 'space-test-unit))
         (unit-rotation 0)
         (foundation (make-instance 'space-test-foundation))
         (space (make-instance 'nsp:space :axial axial
                                          :tokens tokens
                                          :overlay overlay
                                          :unit unit
                                          :unit-rotation unit-rotation
                                          :foundation foundation)))
    (is eqv axial (nsp:axial space))
    (is eqv tokens (nsp:tokens space))
    (is eqv overlay (nsp:overlay space))
    (is eqv unit (nsp:unit space))
    (is eqv unit-rotation (nsp:unit-rotation space))
    (is eqv foundation (nsp:foundation space))
    (is eqv space (reinitialize-instance space))
    (is eqv axial (nsp:axial space))
    (is eqv tokens (nsp:tokens space))
    (is eqv overlay (nsp:overlay space))
    (is eqv unit (nsp:unit space))
    (is eqv unit-rotation (nsp:unit-rotation space))
    (is eqv foundation (nsp:foundation space))
    (let* ((new-axial (nc:axial 1 1))
           (args (list :axial new-axial :tokens '() :overlay nil
                       :unit nil :unit-rotation nil :foundation nil)))
      (is eqv space (apply #'reinitialize-instance space args))
      (is eqv new-axial (nsp:axial space))
      (false (nsp:tokens space))
      (false (nsp:overlay space))
      (false (nsp:unit space))
      (false (nsp:unit-rotation space))
      (false (nsp:foundation space)))))

(define-test space-make-spaces
  (let* ((things (list (nc:axial 0 0)
                       (make-instance 'nsp:space :axial (nc:axial 0 1))))
         (axials (mapcar #'nc:ensure-axial '((0 0) (0 1))))
         (dict (apply #'nsp:make-spaces things)))
    (true (typep dict 'dict))
    (is = 2 (dict-count dict))
    (dolist (axial axials)
      (let ((thing (dict-find dict axial)))
        (true (typep thing 'nsp:space))
        (false (nsp:tokens thing))
        (false (nsp:unit thing))
        (false (nsp:unit-rotation thing))
        (false (nsp:foundation thing)))))
  (let* ((axial (nc:axial 0 0))
         (space-1 (make-instance 'nsp:space :axial axial))
         (space-2 (make-instance 'nsp:space :axial axial))
         (space-3 (make-instance 'nsp:space :axial axial))
         (dict (nsp:make-spaces space-1 space-2 space-3 axial)))
    (is = 1 (dict-count dict))))

;; ;;; TODO
;; (define-test space-edit-space
;;   (let* ((axial (nc:axial 0 0))
;;          (tokens-1 (list (make-instance 'space-test-token)))
;;          (tokens-2 (list (make-instance 'space-test-token)))
;;          (tile-1 (make-instance 'space-test-tile))
;;          (tile-2 (make-instance 'space-test-tile))
;;          (rotation-1 :w)
;;          (rotation-2 :s)
;;          (foundation-1 (make-instance 'space-test-foundation))
;;          (foundation-2 (make-instance 'space-test-foundation))
;;          (space-1 (make-instance 'nsp:space :axial axial :tokens tokens-1
;;                                             :tile tile-1 :rotation rotation-1
;;                                             :foundation foundation-1))
;;          (space-2 (nsp:edit-space space-1 :tokens tokens-2
;;                                           :tile tile-2 :rotation rotation-2
;;                                           :foundation foundation-2)))
;;     (isnt eq space-1 space-2)
;;     (is equalp (nsp:axial space-1) (nsp:axial space-2))
;;     (is eq tokens-1 (nsp:tokens space-1))
;;     (is eq tile-1 (nsp:tile space-1))
;;     (is eq rotation-1 (nsp:rotation space-1))
;;     (is eq foundation-1 (nsp:foundation space-1))
;;     (is eq tokens-2 (nsp:tokens space-2))
;;     (is eq tile-2 (nsp:tile space-2))
;;     (is eq rotation-2 (nsp:rotation space-2))
;;     (is eq foundation-2 (nsp:foundation space-2))))

;; (define-test space-edit-spaces
;;   (let* ((space-1 (make-instance 'nsp:space :axial (nc:axial 0 0)))
;;          (space-2 (make-instance 'nsp:space :axial (nc:axial 0 1)))
;;          (spaces (nsp:make-spaces space-1 space-2)))
;;     (is eq nil (nsp:tile space-1))
;;     (is eq nil (nsp:tile space-2))
;;     (let* ((tile (make-instance 'space-test-tile))
;;            (new-spaces (nsp:edit-spaces spaces space-2 :tile tile))
;;            (new-space (gethash (nc:axial 0 1) new-spaces)))
;;       (is = 2 (hash-table-count new-spaces))
;;       (is eq nil (nsp:tile space-1))
;;       (is eq nil (nsp:tile space-2))
;;       (is eq tile (nsp:tile new-space))))
;;   (let* ((space-1 (make-instance 'nsp:space :axial (nc:axial 0 0)))
;;          (space-2 (make-instance 'nsp:space :axial (nc:axial 0 1)))
;;          (spaces (nsp:make-spaces space-1 space-2)))
;;     (fail (nsp:edit-spaces spaces space-2 :axial (nc:axial 0 0))
;;         nsp:cannot-edit-axial)))

(define-class space-test-other-unit (nt:unit) ())

(define-test space-find-element
  (let* ((token (make-instance 'space-test-token))
         (instant (make-instance 'space-test-instant))
         (unit (make-instance 'space-test-unit))
         (foundation (make-instance 'space-test-foundation)))
    (flet ((make (q r &rest args)
             (apply #'make-instance 'nsp:space :axial (nc:axial q r) args)))
      (let* ((space-1 (make 0 0 :tokens (list token)))
             (space-2 (make 0 1 :overlay instant))
             (space-3 (make 0 2 :unit unit :unit-rotation 0))
             (space-4 (make 0 3 :foundation foundation))
             (dict (nsp:make-spaces space-1 space-2 space-3 space-4)))
        (is eqv space-1 (nsp:find-element dict token))
        (is eqv space-2 (nsp:find-element dict instant))
        (is eqv space-3 (nsp:find-element dict unit))
        (is eqv space-4 (nsp:find-element dict foundation))
        (let ((other-unit (make-instance 'space-test-other-unit)))
          (false (nsp:find-element dict other-unit))))
      (let* ((omega-space (make 0 0 :tokens (list token) :overlay instant
                                    :unit unit :unit-rotation 0
                                    :foundation foundation))
             (dict (nsp:make-spaces omega-space)))
        (is eqv omega-space (nsp:find-element dict token))
        (is eqv omega-space (nsp:find-element dict instant))
        (is eqv omega-space (nsp:find-element dict unit))
        (is eqv omega-space (nsp:find-element dict foundation))
        (let ((other-unit (make-instance 'space-test-other-unit)))
          (false (nsp:find-element dict other-unit)))))))

(define-test space-augment-spaces
  (flet ((make (q r &rest args)
           (apply #'make-instance 'nsp:space :axial (nc:axial q r) args)))
    (let* ((space-1 (make 0 0))
           (space-2 (make 0 1))
           (dict-1 (nsp:make-spaces space-1 space-2))
           (token (make-instance 'space-test-token))
           (space-1-alt (make 0 0 :tokens (list token)))
           (space-3 (make 0 2))
           (dict-2 (nsp:make-spaces space-3))
           (dict-3 (nsp:augment-spaces dict-1 space-1-alt space-2 dict-2)))
      (is = 3 (dict-count dict-3))
      (is eqv space-1-alt (dict-find dict-3 (nc:axial 0 0)))
      (is eqv space-2 (dict-find dict-3 (nc:axial 0 1)))
      (is eqv space-3 (dict-find dict-3 (nc:axial 0 2))))))
