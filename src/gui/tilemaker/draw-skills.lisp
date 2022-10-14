;;;; src/gui/tilemaker/draw-skills.lisp

(in-package #:nervous-island.gui.tilemaker)

(defgeneric draw-skills (state skill &rest skills))

(defmethod draw-skills (state skill &rest skills)
  (declare (ignore state))
  (dolist (skill (cons skill skills))
    (draw-skill skill)))

(defmethod draw-skills (state (skill nsk:undirected) &rest skills)
  ;; WARNING: dangerous implementation. We ignore the passed skills
  ;; and use the ones from ALLOCATED-CORNERS.
  (declare (ignore skills))
  (loop for (skill . corner) in (allocated-corners state)
        do (draw-skill skill :corner corner))
  ;; The previous implementation is below:
  ;; (dolist (skill (cons skill skills))
  ;;   (let ((corner (a:assoc-value (allocated-corners state) skill)))
  ;;     (draw-skill skill :corner corner)))
  )

(defmethod draw-skills (state (skill nsk:armor) &rest skills)
  (dolist (skill (cons skill skills))
    (draw-skill skill :shadowp t))
  (dolist (skill (cons skill skills))
    (draw-skill skill)))

(defmethod draw-skills (state (attack na:attack) &rest attacks)
  (flet ((preprocess (attacks)
           (loop for attack in attacks
                 for strength = (na:strength attack)
                 for copy = (vs:copy attack :strength 1)
                 nconc (make-list strength :initial-element copy))))
    (let* ((attacks (preprocess (cons attack attacks)))
           (direction (nsk:direction attack))
           (allocated-left-p (allocated-left-p state direction))
           (allocated-right-p (allocated-right-p state direction))
           (margin (* 0.15 shapes:*side*))
           (initial-offset (* 0.25 shapes:*side*))
           (begin (- initial-offset (if allocated-right-p margin 0)))
           (end (+ (- initial-offset) (if allocated-left-p margin 0)))
           (count (1+ (length attacks))))
      (loop for i from 0 below count
            for attack in attacks
            do (let* ((direction (nsk:direction attack))
                      (rotation (1- (position direction ncom:*directions*))))
                 (v:with-graphics-state
                   (v:rotate (* rotation pi -1/3))
                   (let ((new-x (a:lerp (/ (1+ i) count) begin end)))
                     (v:translate new-x 0))
                   (draw-skill attack
                               :allocated-left-p allocated-left-p
                               :allocated-right-p allocated-right-p)))))))

(defmethod draw-skills (state (skill ne:effect) &rest skills)
  (let* ((to-draw (remove-duplicates (cons skill skills) :key #'class-of))
         (no-plus-type '(or ne:medic ne:move-doubler ne:quartermaster
                         ne:additional-initiative ne:scoper))
         (plusp (not (find-if (a:rcurry #'typep no-plus-type) to-draw))))
    (labels ((draw (x y)
               (v:with-graphics-state
                 (v:translate (* shapes:*side* x) (* shapes:*side* y))
                 (draw-skill (pop to-draw))))
             (plus (x x2)
               (v:translate (* shapes:*side* (- x)) 0)
               (v:scale 0.7 0.7)
               (if (typep skill 'ne:saboteur)
                   (shapes:minus)
                   (shapes:medic))
               (v:scale (/ 0.7) (/ 0.7))
               (v:translate (* shapes:*side* (+ x x2)) 0)))
      (v:with-graphics-state
        (ecase (length to-draw)
          (1 (when plusp (plus 0.25 0.04))
           (v:scale 1.5 1.5)
           (draw 0 0))
          (2 (when plusp (plus 0.18 0))
           (draw 0 0.15)
           (draw 0 -0.15))
          (3 (when plusp (plus 0.25 0))
           (draw -0.09 0.15)
           (draw -0.09 -0.15)
           (draw 0.17 0)))))))
