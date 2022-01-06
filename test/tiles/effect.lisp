;;;; test/tiles/effect.lisp

(in-package #:nervous-island/test)

(defclass effect-test-effect (ne:effect) ())

(define-test effect-instantiation
  (flet ((test (class)
           (fail (make-instance class) p:protocol-object-instantiation)))
    (mapcar #'test '(ne:effect ne:undirected-effect ne:numeric-effect
                     ne:melee-officer ne:ranged-officer ne:scout ne:mother
                     ne:medic ne:transport ne:quartermaster ne:recon-center
                     ne:scoper ne:saboteur)))
  (fail (make-instance 'ne:directed-effect :direction :w)
      p:protocol-object-instantiation)
  (true (make-instance 'effect-test-effect)))
