(in-package #:nervous-island.gui.shapes)

(defun %mobility (side fillp)
  (when fillp (v:set-rgba-fill 1 1 1 1))
  ;;(v:translate (* -0.05 side) 0)
  ;; Triangle
  (v:move-to (* 0.03 side) (* -0.01 side))
  (v:line-to (* 0.03 side) (* 0.08 side))
  (v:line-to (* -0.06 side) (* 0.03 side))
  (if fillp (v:fill-path))
  ;; Arrow bottom
  (v:move-to (* -0.04 side) (* 0.01 side))
  (v:quadratic-to (* -0.08 side) (* -0.07 side)
                  (* 0.01 side) (* -0.09 side))
  (v:quadratic-to (* -0.04 side) (* -0.04 side)
                  (* 0.005 side) (* -0.01 side))
  (when fillp (v:fill-path)))

(defun mobility (&optional (side *side*) (fillp t))
  (v:with-graphics-state
    (%mobility side fillp)))
