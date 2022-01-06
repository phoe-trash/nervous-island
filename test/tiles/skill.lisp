;;;; test/tiles/skill.lisp

(in-package #:nervous-island/test)

(defclass skill-test-directed (nsk:directed) ())

(defclass skill-test-active (nsk:active) ())

(define-test skill-instantiation
  (flet ((test (class)
           (fail (make-instance class) p:protocol-object-instantiation)))
    (mapcar #'test '(nsk:skill nsk:active nsk:passive nsk:undirected
                     nsk:active-undirected nsk:passive-undirected)))
  (flet ((test (class)
           (fail (make-instance class :direction :w)
               p:protocol-object-instantiation)))
    (mapcar #'test '(nsk:directed nsk:active-directed nsk:passive-directed)))
  (fail (make-instance 'skill-test-directed :direction 0) 'type-error)
  (fail (make-instance 'skill-test-active :activation-time 0) 'type-error)
  (true (make-instance 'skill-test-active :activation-time :turn))
  (true (make-instance 'skill-test-directed :direction :w)))
