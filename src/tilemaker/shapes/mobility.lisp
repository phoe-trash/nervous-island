(in-package #:nervous-island.tilemaker.shapes)

(defun mobility (&optional (side *side*))
  (v:with-graphics-state
    (v:set-rgba-fill 1 1 1 1)
    (v:translate (* -0.05 side) 0)
    ;; Triangle
    (v:move-to (* 0.08 side) (* -0.01 side))
    (v:line-to (* 0.08 side) (* 0.08 side))
    (v:line-to (* -0.01 side) (* 0.03 side))
    (v:fill-path)
    ;; Arrow botton
    (v:move-to (* 0.01 side) (* 0.03 side))
    (v:quadratic-to (* -0.03 side) (* -0.07 side)
                    (* 0.06 side) (* -0.09 side))
    (v:quadratic-to (* 0.01 side) (* -0.04 side)
                    (* 0.07 side) (* 0.01 side))
    (v:fill-path)))
