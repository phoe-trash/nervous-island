(in-package #:nervous-island.gui.shapes)

(defun attack-decoration (&optional (side *side*))
  (let ((height (* side (sqrt 3))))
    (v:translate 0 (* 0.403 height))
    (v:set-rgba-fill 0.95 0.95 0.85 1)
    (v:arc 0 0 (* height 0.03) pi (* 2 pi))
    (v:fill-path)))

(defun melee-shadow (&optional (side *side*) pointsp)
  (let ((height (* side (sqrt 3)))
        (width (* side 2)))
    (v:with-graphics-state
      (flet ((operate (sign)
               (let ((start-x (* sign 0.047 width))
                     (start-y (* 0.345 height))
                     (stop-x (* sign 0.073 width))
                     (stop-y (* 0.32 height)))
                 ;; Illustrate the points
                 (when pointsp
                   (v:set-rgb-fill 0 1 0)
                   (v:arc start-x start-y 5 0 (* 2 pi))
                   (v:fill-path)
                   (v:set-rgb-fill 1 0 0)
                   (v:arc stop-x stop-y 5 0 (* 2 pi))
                   (v:fill-path))
                 ;; Actual shadow
                 (v:set-gradient-fill start-x start-y 0 0 0 1
                                      stop-x stop-y 0 0 0 0
                                      :extend-start nil
                                      :extend-end t))
               (v:move-to 0 (* 0.2 height))
               (v:line-to 0 (* 0.28 height))
               (v:line-to (* sign 0.09 width) (* 0.4 height))
               (v:line-to (* sign 0.2 width) (* 0.4 height))
               (v:line-to (* sign 0.2 width) (* 0.4 height))
               (v:line-to 0 (* 0.2 height))
               (v:fill-path)))
        (operate +1)
        (operate -1)))))

(defun melee-body (&optional (side *side*) pointsp)
  (let ((height (* side (sqrt 3)))
        (width (* side 2)))
    (v:with-graphics-state
      (v:set-rgba-fill 0 0 0 (if pointsp 0.5 1))
      (v:set-rgba-stroke 0.95 0.95 0.85 (if pointsp 0.5 1))
      (v:set-line-width (* height 0.01))
      (v:move-to 0 (* 0.26 height))
      (v:line-to (* 0.09 width) (* 0.403 height))
      (v:line-to (* -0.09 width) (* 0.403 height))
      (v:line-to 0 (* 0.26 height))
      (v:fill-and-stroke))))

(defun melee (&optional (side *side*) pointsp (shadowp t))
  (when shadowp (melee-shadow side pointsp))
  (melee-body side pointsp)
  (attack-decoration))

(defun ranged-body (&optional (side *side*) pointsp)
  (let ((height (* side (sqrt 3)))
        (width (* side 2)))
    (v:with-graphics-state
      (v:set-rgba-fill 0 0 0 (if pointsp 0.5 1))
      (v:set-rgba-stroke 0.95 0.95 0.85 (if pointsp 0.5 1))
      (v:set-line-width (* height 0.01))
      (v:move-to 0 (* 0.2 height))
      (v:line-to (* 0.05 width) (* 0.403 height))
      (v:line-to (* -0.05 width) (* 0.403 height))
      (v:line-to 0 (* 0.2 height))
      (v:fill-and-stroke))))

(defun ranged (&optional (side *side*) pointsp (shadowp t))
  (when shadowp (ranged-shadow side pointsp))
  (ranged-body side pointsp)
  (attack-decoration))

(defun ranged-shadow (&optional (side *side*) pointsp)
  (let ((height (* side (sqrt 3)))
        (width (* side 2)))
    (v:with-graphics-state
      (flet ((operate (sign)
               (let ((start-x (* sign 0.02 width))
                     (start-y (* 0.33 height))
                     (stop-x (* sign 0.064 width))
                     (stop-y (* 0.31 height)))
                 ;; Illustrate the points
                 (when pointsp
                   (v:set-rgb-fill 0 1 0)
                   (v:arc start-x start-y 5 0 (* 2 pi))
                   (v:fill-path)
                   (v:set-rgb-fill 1 0 0)
                   (v:arc stop-x stop-y 5 0 (* 2 pi))
                   (v:fill-path))
                 ;; Actual shadow
                 (v:set-gradient-fill start-x start-y 0 0 0 1
                                      stop-x stop-y 0 0 0 0
                                      :extend-start nil
                                      :extend-end t))
               (v:move-to 0 (* 0.12 height))
               (v:line-to 0 (* 0.28 height))
               (v:line-to (* sign 0.04 width) (* 0.4 height))
               (v:line-to (* sign 0.2 width) (* 0.4 height))
               (v:line-to (* sign 0.2 width) (* 0.4 height))
               (v:line-to 0 (* 0.12 height))
               (v:fill-path)))
        (operate +1)
        (operate -1)))))

(defun gauss-body (&optional (side *side*))
  (let ((height (* side (sqrt 3)))
        (width (* side 2)))
    (v:with-graphics-state
      (flet ((make-triangle (&optional (r 0.95) (g 0.95) (b 0.85)
                               (line-width (* height 0.01)))
               (v:set-rgba-fill 0.95 0.95 0.85 1)
               (v:set-rgba-stroke r g b 1)
               (v:set-line-width line-width)
               (v:move-to 0 (* 0.2 height))
               (v:line-to (* 0.05 width) (* 0.403 height))
               (v:line-to (* -0.05 width) (* 0.403 height))
               (v:line-to 0 (* 0.2 height))
               (v:fill-and-stroke)))
        (make-triangle)
        (v:translate 0 (* 0.045 height))
        (make-triangle 0 0 0 (* height 0.015))))))

(defun gauss (&optional (side *side*) pointsp (shadowp t))
  (when shadowp (ranged-shadow side pointsp))
  (gauss-body side))
