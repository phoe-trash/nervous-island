(in-package #:nervous-island.gui.shapes)

(defun medic (&optional (side *side*))
  (let ((ratio (* side 0.04)))
    (v:with-graphics-state
      (v:set-rgba-fill 1 1 1 1)
      (v:rectangle (* ratio -0.5) (* ratio -2) (* ratio 1) (* ratio 4))
      (v:rectangle (* ratio -2) (* ratio -0.5) (* ratio 4) (* ratio 1))
      (v:fill-path))))
