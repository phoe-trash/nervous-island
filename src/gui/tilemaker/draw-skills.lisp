(in-package #:nervous-island.gui.tilemaker)

(defgeneric draw-skills (state skill &rest skills))

(defmethod draw-skills (state (skill nsk:undirected) &rest skills)
  (dolist (skill (cons skill skills))
    (let ((corner (a:assoc-value (allocated-corners state) skill)))
      (draw-skill skill :corner corner))))

(defmethod draw-skills (state skill &rest skills)
  (declare (ignore state))
  (dolist (skill (cons skill skills))
    (draw-skill skill)))

(defmethod draw-skills (state (skill nsk:armor) &rest skills)
  (dolist (skill (cons skill skills))
    (draw-skill skill :shadowp t))
  (dolist (skill (cons skill skills))
    (draw-skill skill)))

(defmethod draw-skills (state (attack na:attack) &rest attacks)
  (flet ((preprocess (attacks)
           (loop for attack in attacks
                 for strength = (na:strength attack)
                 for copy = (φ::copy-object attack :strength 1)
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
