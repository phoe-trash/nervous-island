;;;; test/tiles/effect.lisp

(in-package #:nervous-island/test)

(define-test effect-instantiation-negative
  (flet ((test (class)
           (fail (make-instance class) p:protocol-object-instantiation)))
    (mapcar #'test '(ne:effect ne:undirected-effect ne:numeric
                     ne:melee-officer ne:ranged-officer ne:speed
                     ne:additional-initiative ne:medic ne:mobility ne:quartermaster
                     ne:move-doubler ne:scoper ne:saboteur)))
  (fail (make-instance 'ne:directed-effect :direction :w)
      p:protocol-object-instantiation))

(defun find-effects (name)
  (let ((classes (class-direct-subclasses (find-class name))))
    (loop for class in classes
          for package = (symbol-package (class-name class))
          when (eqv package (find-package '#:ne))
            collect class)))

(define-test effect-instantiation-directed
  (dolist (class (find-effects 'ne:directed-effect))
    (let ((effect (make-instance class :direction :w :strength 42
                                       :allow-other-keys t)))
      (is eqv :w (nsk:direction effect))
      (when (typep effect 'ne:numeric)
        (is eqv 42 (ne:strength effect))))))

(define-test effect-instantiation-undirected
  (dolist (class (find-effects 'ne:undirected-effect))
    (let ((effect (make-instance class :strength 42
                                       :allow-other-keys t)))
      (when (typep effect 'ne:numeric)
        (is eqv 42 (ne:strength effect))))))
