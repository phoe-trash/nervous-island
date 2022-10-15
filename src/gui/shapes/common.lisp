(in-package #:nervous-island.gui.shapes)

(defvar *side*)

(defun hexagon (&optional (side *side*))
  (let ((half-height (* side 1d0 (sqrt 3) 1/2)))
    (v:move-to (- side) 0)
    (v:line-to (- (/ side 2)) (+ half-height))
    (v:line-to (+ (/ side 2)) (+ half-height))
    (v:line-to (+ side) 0)
    (v:line-to (+ (/ side 2)) (- half-height))
    (v:line-to (- (/ side 2)) (- half-height))
    (v:line-to (- side) 0)))

(defun background (&optional (r 0.5) (g 0.5) (b 0.5) (a 1))
  (v:with-graphics-state
    (v:set-rgba-fill r g b a)
    (hexagon)
    (v:fill-path)))

(defun bg-image (path x-offset y-offset)
  (let* ((bg (i:read-png path))
         (iheight (i:image-height bg))
         (iwidth (i:image-width bg))
         (x-offset (round (- x-offset (* 1/2 iwidth))))
         (y-offset (round (- y-offset (* 1/2 iheight)))))
    (v:compose bg x-offset y-offset)))

(defun outer-shadow ()
  (flet ((triangle-blur (intensity)
           (let* ((side *side*)
                  (half-height (* side 1d0 (sqrt 3) 1/2)))
             (v:set-gradient-fill 0 (1+ (* 0.8 half-height)) 0 0 0 1
                                  0 (* (- 1 intensity) 0.8 half-height) 0 0 0 0
                                  :extend-start nil
                                  :extend-end t)
             (v:move-to 0 0)
             (v:line-to (- (/ side 2)) (+ half-height))
             (v:line-to (+ (/ side 2)) (+ half-height))
             (v:line-to 0 0)
             (v:fill-path))))
    (v:with-graphics-state
      (dotimes (i 6)
        (triangle-blur 0.1)
        (v:rotate (/ pi 3))))))

(defun outer-hexagon (&optional (side *side*))
  (v:with-graphics-state
    (v:set-rgba-fill 0 0 0 1)
    (hexagon side)
    (hexagon (* 81/100 side))
    (v:even-odd-fill)))

(defun outer-decoration (&optional (side *side*))
  (v:with-graphics-state
    (v:set-rgba-fill 0.95 0.95 0.85 1)
    (hexagon (* 815/1000 side))
    (hexagon (* 80/100 side))
    (v:even-odd-fill)
    (dotimes (i 6)
      (v:with-graphics-state
        (v:rotate (* i pi 1/3))
        (v:translate (* 0.795 side) 0)
        (hexagon (* 0.02 side))
        (v:fill-path)))))

(defun inverse-sin-domain (x)
  (vecto:linear-domain (sin (* (- x) pi))))

(defmacro with-hex-tile
    ((side-var height-var width-var)
     (&key
        (height 800)
        (background-color '(0.5 0.5 0.5 1))
        save-path
        bg-image (bg-x-offset 0) (bg-y-offset 0)
        (instantp nil))
     &body body)
  (a:once-only (bg-image save-path)
    `(let* ((,height-var ,height)
            (,width-var (round (* ,height-var 2 (/ (sqrt 3)))))
            (,side-var (/ ,width-var 2))
            (*side* ,side-var))
       (v:with-canvas (:width ,width-var :height ,height-var)
         (v:translate (floor ,width-var 2) (floor ,height-var 2))
         (apply #'background ,background-color)
         (when ,bg-image (bg-image ,bg-image ,bg-x-offset ,bg-y-offset))
         ,@(unless instantp `((outer-shadow)))
         ,@body
         ,@(unless instantp `((outer-hexagon)))
         ,@(unless instantp `((outer-decoration)))
         (when ,save-path
           (v:save-png ,save-path))
         (v:zpng-object)))))
