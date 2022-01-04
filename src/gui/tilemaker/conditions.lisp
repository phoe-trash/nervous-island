(in-package #:nervous-island.gui.tilemaker)

(define-condition drawing-an-unknown-skill (warning)
  ((%skill :reader drawing-an-unknown-skill-skill :initarg :skill))
  (:report (lambda (condition stream)
             (format stream "Don't know how to draw skill ~S."
                     (drawing-an-unknown-skill-skill condition)))))

(define-condition remaining-skill-after-drawing (warning)
  ((%skill :reader remaining-skill-after-drawing-skill :initarg :skill))
  (:report (lambda (condition stream)
             (format stream "Skill remains after completing drawing: ~S"
                     (remaining-skill-after-drawing-skill condition)))))
