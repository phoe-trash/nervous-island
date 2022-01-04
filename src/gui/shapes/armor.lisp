(in-package #:nervous-island.gui.shapes)

(defun armor-path (&optional (side *side*))
  (let ((x (* 0.5 side)))
    (v:move-to x x)
    (v:line-to (* -1 x) x)
    (v:line-to (* -1 x) (* -3 x))
    (v:line-to x (* -3 x))
    (v:line-to x x)
    (v:clip-path)
    (v:end-path-no-op)))

(defun armor-shadow (&optional (side *side*))
  (v:with-graphics-state
    (v:translate 0 (* 0.7 side))
    (v:scale 1 0.1)
    (flet ((fn (x) (vecto:linear-domain (sin (* (- x) pi)))))
      (v:set-gradient-fill
       0 0 0 0 0 1
       0 (* -0.9 side) 0 0 0 0
       :domain-function #'fn
       :coordinates-function 'vecto:polar-coordinates)
      (armor-path)
      (v:arc 0 0 (* 1.4 side) 0 (* 2 pi))
      (v:fill-path))))

(defun armor (&optional (side *side*))
  (v:with-graphics-state
    (v:translate 0 (* 0.7 side))
    (v:scale 1 0.1)
    (armor-path)
    (v:set-rgba-fill 1 1 1 1)
    (v:arc 0 0 (* 0.7 side) 0 (* 2 pi))
    (v:fill-path)))
