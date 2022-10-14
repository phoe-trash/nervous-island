;;;; src/gui/tilemaker/draw-skill.lisp

(in-package #:nervous-island.gui.tilemaker)

(defgeneric draw-skill (skill &key &allow-other-keys))

(defmethod draw-skill (skill &key)
  (warn 'drawing-an-unknown-skill :skill skill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directed skills

(defmethod draw-skill ((net nsk:net) &key)
  (let* ((direction (nsk:direction net))
         (rotation (1- (position direction ncom:*directions*))))
    (v:with-graphics-state
      (v:rotate (* rotation pi -1/3))
      (shapes:net))))

(defmethod draw-skill ((skill nsk:armor) &key shadowp)
  (let* ((direction (nsk:direction skill))
         (rotation (1- (position direction ncom:*directions*))))
    (v:with-graphics-state
      (v:rotate (* rotation pi -1/3))
      (if shadowp
          (shapes:armor-shadow)
          (shapes:armor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attacks

(defmethod draw-skill ((attack na:melee) &key) (shapes:melee))

(defmethod draw-skill ((attack na:ranged) &key) (shapes:ranged))

(defmethod draw-skill ((attack na:gauss-cannon) &key) (shapes:gauss))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undirected skills

(defmethod draw-skill :around ((skill nsk:undirected) &key corner)
  (check-type corner ncom:diagonal)
  (let* ((rotation (position corner ncom:*diagonals*)))
    (v:with-graphics-state
      (v:rotate (* rotation pi -1/3))
      (v:translate (* -0.55 shapes:*side*) 0)
      (shapes:ability-circle)
      (v:rotate (* rotation pi 1/3))
      (call-next-method))))

(defmethod draw-skill ((skill nsk:initiative) &key)
  (shapes:text (nsk:value skill)))

(defmethod draw-skill ((skill nsk:mobility) &key)
  (shapes:mobility))

(defmethod draw-skill ((skill nsk:toughness) &key)
  (shapes:toughness))

(defmethod draw-skill ((skill nsk:explosion) &key)
  (shapes:bomb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Effects

(defmethod draw-skill ((skill ne:medic) &key)
  (shapes:medic))

(defmethod draw-skill ((skill ne:melee-officer) &key)
  (v:with-graphics-state
    (v:translate 0 (* -0.46 shapes:*side*))
    (v:scale 0.8 0.8)
    (shapes:melee shapes:*side* nil nil)))

(defmethod draw-skill ((skill ne:ranged-officer) &key)
  (v:with-graphics-state
    (v:translate 0 (* -0.43 shapes:*side*))
    (v:scale 0.8 0.8)
    (shapes:ranged shapes:*side* nil nil)))

(defmethod draw-skill ((skill ne:speed) &key)
  (v:with-graphics-state
    (shapes:ability-circle shapes:*side* nil nil)
    (shapes:text (ne:strength skill))))
