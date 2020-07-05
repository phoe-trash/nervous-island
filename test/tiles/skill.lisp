;;;; test/armies/skill.lisp

(in-package #:nervous-island.test)

(defclass skill-test-directed (ns:directed) ())

(defclass skill-test-active (ns:active) ())

(define-test skill-instantiation
  (flet ((test (class)
           (fail (make-instance class) p:protocol-object-instantiation)))
    (mapcar #'test '(ns:skill ns:active ns:passive ns:undirected
                     ns:active-undirected ns:passive-undirected)))
  (flet ((test (class)
           (fail (make-instance class :direction :w)
               p:protocol-object-instantiation)))
    (mapcar #'test '(ns:directed ns:active-directed ns:passive-directed)))
  (fail (make-instance 'skill-test-directed :direction 0) 'type-error)
  (fail (make-instance 'skill-test-active :activation-time 0) 'type-error))
