(in-package #:nervous-island.tilemaker.shapes)

(defun bomb-body (&optional (side *side*))
  (v:arc (* -0.02 side) (* -0.02 side)
         (* 0.06 side) 0 (* 2 pi))
  (v:fill-path))

(defun bomb-fuse (&optional (side *side*))
  (v:with-graphics-state
    (v:translate (* 0.02 side) (* 0.02 side))
    (v:move-to 0 0)
    (v:set-line-width (* 0.008 side))
    (v:curve-to (* 0.03 side) 0
                0 (* 0.03 side)
                (* 0.03 side) (* 0.04 side))
    (v:curve-to 0 (* 0.03 side)
                (* 0.03 side) 0
                0 0)
    (v:fill-and-stroke)))

(defun bomb (&optional (side *side*))
  (v:with-graphics-state
    (v:set-rgba-fill 1 1 1 1)
    (v:set-rgba-stroke 1 1 1 1)
    (bomb-body)
    (bomb-fuse)
    (v:with-graphics-state
      (v:translate (* 0.045 side) (* 0.06 side))
      (v:rotate (* pi -5/8))
      (dotimes (i 5)
        (v:with-graphics-state
          (v:rotate (* i pi 1/4))
          (v:rectangle (* -0.004 side) (* 0.015 side)
                       (* 0.008 side) (* 0.015 side))
          (v:fill-path))))))