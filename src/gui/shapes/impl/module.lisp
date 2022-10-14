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

(defun module-ring (&optional (side *side*) backgroundp)
  (flet ((ring (offset color)
           (v:with-graphics-state
             (apply #'v:set-rgba-fill color)
             (v:arc 0 0 (* side (+ 0.58 offset)) 0 (* 2 pi))
             (v:arc 0 0 (* side (- 0.55 offset)) 0 (* 2 pi))
             (v:even-odd-fill))))
    (if backgroundp
        (ring 0.01 '(0 0 0 1))
        (ring 0.0 '(0.95 0.95 0.85 1)))))

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
  (module-circle-shadow side)
  (module-ring-shadow-outer)
  (module-ring-shadow-inner)
  (module-ring side t))

(defun module-range-shadow (&optional (direction 0) (side *side*))
  (v:with-graphics-state
    (v:rotate (* (1- direction) pi -1/3))
    (v:with-graphics-state
      (v:set-rgba-fill 0.95 0.95 0.85 1)
      (v:set-rgba-stroke 0 0 0 1)
      (v:set-line-width (* side 0.008))
      (v:move-to (* 0.09 side) (* 0.4 side))
      (v:line-to (* 0.09 side) (* 0.8 side))
      (v:line-to (* -0.09 side) (* 0.8 side))
      (v:line-to (* -0.09 side) (* 0.4 side))
      (v:line-to (* 0.09 side) (* 0.4 side))
      (v:fill-and-stroke))))

(defun module-arrow (&optional (triangles 1) (side *side*) (color '(0 0 0 1)))
  (v:with-graphics-state
    (v:translate 0 (* side (+ 0.63 (* 0.03 triangles))))
    (v:scale 1.2 1.2)
    (apply #'v:set-rgba-fill color)
    (let ((triangle-side (* 0.1 side)))
      (dotimes (i triangles)
        (v:move-to 0 0)
        (v:line-to (* 1/2 triangle-side)
                   (* -1/2 triangle-side (sqrt 3)))
        (v:line-to (* -1/2 triangle-side)
                   (* -1/2 triangle-side (sqrt 3)))
        (v:line-to 0 0)
        (v:fill-path)
        (v:translate 0 (* -0.5 triangle-side)))
      (v:move-to (* 0.2 triangle-side) 0)
      (v:line-to (* 0.2 triangle-side) (* -0.8 triangle-side))
      (v:line-to (* -0.2 triangle-side) (* -0.8 triangle-side))
      (v:line-to (* -0.2 triangle-side) 0)
      (v:line-to (* 0.2 triangle-side) 0)
      (v:fill-path))))

(defun module-range
    (&optional (direction 0) (triangles 1) (side *side*))
  (v:with-graphics-state
    (v:rotate (* (1- direction) pi -1/3))
    (v:with-graphics-state
      (v:set-rgba-fill 0.95 0.95 0.85 1)
      (v:move-to (* 0.09 side) (* 0.4 side))
      (v:line-to (* 0.09 side) (* 0.8 side))
      (v:line-to (* -0.09 side) (* 0.8 side))
      (v:line-to (* -0.09 side) (* 0.4 side))
      (v:line-to (* 0.09 side) (* 0.4 side))
      (v:fill-path))
    (module-arrow triangles)))

(defun star ()
  (v:with-graphics-state
    (v:scale 0.1 0.1)
    (v:set-rgba-fill 1 1 1 1)
    (v:move-to (* *side* (sin (* 0 pi 4/5)))
               (* *side* (cos (* 0 pi 4/5))))
    (dotimes (i 6)
      (v:line-to (* *side* (sin (* (1+ i) pi 4/5)))
                 (* *side* (cos (* (1+ i) pi 4/5)))))
    (v:fill-path)))
