(in-package #:nervous-island.gui.shapes)

(defun module-ring-shadow-outer (&optional (side *side*))
  (v:with-graphics-state
    (v:set-gradient-fill
     0 0 0 0 0 1
     (* side 0.48) 0 0 0 0 0
     :domain-function (lambda (x) (v:linear-domain (* 3.5 (- x 1.1))))
     :coordinates-function 'v:polar-coordinates)
    (v:arc 0 0 (* side 0.75) 0 (* 2 pi))
    (v:arc 0 0 (* side 0.58) 0 (* 2 pi))
    (v:even-odd-fill)))

(defun module-ring-shadow-inner (&optional (side *side*))
  (v:with-graphics-state
    (v:set-gradient-fill
     0 0 0 0 0 0
     (* side 0.48) 0 0 0 0 1
     :domain-function (lambda (x) (v:linear-domain (* 3.5 (- x 0.9))))
     :coordinates-function 'v:polar-coordinates)
    (v:arc 0 0 (* side 0.55) 0 (* 2 pi))
    (v:fill-path)))

(defun module-ring (&optional (side *side*))
  (flet ((ring (offset color)
           (v:with-graphics-state
             (apply #'v:set-rgba-fill color)
             (v:arc 0 0 (* side (+ 0.58 offset)) 0 (* 2 pi))
             (v:arc 0 0 (* side (- 0.55 offset)) 0 (* 2 pi))
             (v:even-odd-fill))))
    (ring 0.01 '(0 0 0 1))
    (ring 0.0 '(0.95 0.95 0.85 1))))

(defun module-circle-shadow (&optional (side *side*) (scale 3.0))
  (v:with-graphics-state
    (v:scale scale scale)
    (circle-shadow (* 0.85 side))))

(defun module-circle (&optional (side *side*) (scale 3.0))
  (v:with-graphics-state
    (v:scale scale scale)
    (circle-outer side '(1 1 1 1) 0.4)
    (circle-inner)
    (dotimes (i 8)
      (v:with-graphics-state
        (v:rotate (* i 1/8 2 pi))
        (circle-decoration-square side '(1 1 1 1))))))

(defun module-background (&optional (side *side*))
  (module-circle-shadow)
  (module-ring-shadow-outer)
  (module-ring-shadow-inner)
  (module-ring))

(defun module-range (&optional (i 0) (side *side*))
  (let ((height (* side (sqrt 3)))
        (width (* side 1.0)))
    (v:with-graphics-state
      (v:rotate (* (1- i) pi -1/3))
      (v:set-rgba-fill 0 0 0 1)
      (v:set-rgba-stroke 0.95 0.95 0.85 1)
      (v:set-line-width (* height 0.01))
      (v:move-to (* 0.09 width) (* -0.403 height))
      (v:line-to (* 0.09 width) (* 0.403 height))
      (v:line-to (* -0.09 width) (* 0.403 height))
      (v:line-to (* -0.09 width) (* -0.403 height))
      (v:line-to (* 0.09 width) (* -0.403 height))
      (v:fill-and-stroke))))
