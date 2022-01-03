(uiop:define-package #:nervous-island.tilemaker.shapes
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:v #:vecto)
   (#:i #:imago)
   (#:z #:zpng))
  (:export #:*side*
           #:with-hex-tile
           #:net #:melee #:ranged #:gauss))

(in-package #:nervous-island.tilemaker.shapes)

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

(defun background (&optional (r 0.5) (g 0.5) (b 0.5))
  (v:with-graphics-state
    (v:set-rgb-fill r g b)
    (hexagon)
    (v:fill-path)))

(defun triangle-blur (intensity &optional (r 0) (g 0) (b 0) (side *side*))
  (let ((half-height (* side 1d0 (sqrt 3) 1/2)))
    (v:set-gradient-fill 0 (1+ (* 0.8 half-height)) r g b 1
                         0 (* (- 1 intensity) 0.8 half-height) r g b 0
                         :extend-start nil
                         :extend-end t)
    (v:move-to 0 0)
    (v:line-to (- (/ side 2)) (+ half-height))
    (v:line-to (+ (/ side 2)) (+ half-height))
    (v:line-to 0 0)
    (v:fill-path)))

(defun outer-shadow ()
  (v:with-graphics-state
    (dotimes (i 6)
      (triangle-blur 0.1)
      (v:rotate (/ pi 3)))))

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

(defun bg-image (path x-offset y-offset)
  (let* ((bg (i:read-png path))
         (iheight (i:image-height bg))
         (iwidth (i:image-width bg))
         (x-offset (round (- x-offset (* 1/2 iwidth))))
         (y-offset (round (- y-offset (* 1/2 iheight)))))
    (v:compose bg x-offset y-offset)))

(defun net (&optional (side *side*))
  (v:with-graphics-state
    (v:translate 0 (* 0.32 side))
    ;; Clip path
    (let ((x (* 0.5 side)))
      (v:move-to x (* 1 x))
      (v:line-to (* -1 x) (* 1 x))
      (v:line-to (* -1 x) (* -1 x))
      (v:line-to x (* -1 x))
      (v:line-to x (* 1 x))
      (v:clip-path)
      (v:end-path-no-op))
    (v:rotate (* pi 1/4))
    (labels ((make-lines (offset)
               (dotimes (i 9)
                 (let ((start (+ (* 0.1 side) (* 0.05 side (- i 1.5)))))
                   ;; One direction
                   (v:move-to (+ start offset) (- offset))
                   (v:line-to (+ start offset) (+ (* 0.5 side) offset))
                   (v:line-to (- start offset) (+ (* 0.5 side) offset))
                   (v:line-to (- start offset) (- offset))
                   (v:line-to (+ start offset) (- offset))
                   (v:fill-path)
                   ;; The other direction
                   (v:move-to (- offset) (+ start offset))
                   (v:line-to (+ (* 0.5 side) offset) (+ start offset))
                   (v:line-to (+ (* 0.5 side) offset) (- start offset))
                   (v:line-to (- offset) (- start offset))
                   (v:line-to (- offset) (+ start offset))
                   (v:fill-path)))))
      (v:scale 1.4 1.4)
      (v:set-rgba-fill 1 1 1 1)
      (make-lines (* 0.014 side))
      (v:set-rgba-fill 0 0 0 1)
      (make-lines (* 0.006 side)))))

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

(defun melee (&optional (side *side*) pointsp)
  (melee-shadow side pointsp)
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

(defun ranged (&optional (side *side*) pointsp)
  (ranged-shadow side pointsp)
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

(defun gauss (&optional (side *side*) pointsp)
  (ranged-shadow side pointsp)
  (gauss-body side))

(defmacro with-hex-tile
    ((side-var height-var width-var)
     (&key
        (height 800)
        (background-color '(0.5 0.5 0.5))
        save-path
        bg-image (bg-x-offset 0) (bg-y-offset 0))
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
         (outer-shadow)
         ,@body
         (outer-hexagon)
         (outer-decoration)
         (when ,save-path
           (v:save-png ,save-path))
         (v:zpng-object)))))

(uiop:define-package #:nervous-island.tilemaker
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:v #:vecto)
                    (#:φ #:phoe-toolbox)
                    (#:na #:nervous-island.attack)
                    (#:ncom #:nervous-island.common)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:shapes #:nervous-island.tilemaker.shapes))
  (:export #:with-hex-tile))

(in-package #:nervous-island.tilemaker)

(define-condition drawing-an-unknown-skill (warning)
  ((%skill :reader drawing-an-unknown-skill-skill :initarg :skill))
  (:report (lambda (condition stream)
             (format stream "Don't know how to draw skill ~S."
                     (drawing-an-unknown-skill-skill condition)))))

(defgeneric draw-skill (skill &key &allow-other-keys)
  (:method (skill &key)
    (warn 'drawing-an-unknown-skill :skill skill)))

(defmethod draw-skill ((net nsk:net) &key)
  (let* ((direction (nsk:direction net))
         (rotation (1- (position direction ncom:*directions*))))
    (v:with-graphics-state
      (v:rotate (* rotation pi -1/3))
      (shapes:net))))

(defmethod draw-skill ((attack na:melee) &key)
  (shapes:melee))

(defmethod draw-skill ((attack na:ranged) &key)
  (shapes:ranged))

(defmethod draw-skill ((attack na:gauss-cannon) &key)
  (shapes:gauss))

(defgeneric draw-skills (state skill &rest skills)
  (:method (state skill &rest skills)
    (declare (ignore state))
    (dolist (skill (cons skill skills))
      (draw-skill skill))))

(defmethod draw-skills (state (attack na:attack) &rest attacks)
  (flet ((preprocess (attacks)
           (loop for attack in attacks
                 for strength = (na:strength attack)
                 for copy = (ncom::shallow-copy-object attack :strength 1)
                 nconc (make-list strength :initial-element copy))))
    (let* ((attacks (preprocess (cons attack attacks)))
           (direction (nsk:direction attack))
           (occupied-left-p (occupied-left-p state direction))
           (occupied-right-p (occupied-right-p state direction))
           (margin (* 0.1 shapes:*side*))
           (initial-offset (* 0.25 shapes:*side*))
           (begin (- initial-offset (if occupied-right-p margin 0)))
           (end (+ (- initial-offset) (if occupied-left-p margin 0)))
           (count (1+ (length attacks))))
      (loop for i from 0 below count
            for attack in attacks
            do (let* ((direction (nsk:direction attack))
                      (rotation (1- (position direction ncom:*directions*))))
                 (v:with-graphics-state
                   (v:rotate (* rotation pi -1/3))
                   (let ((new-x (a:lerp (/ (1+ i) count) begin end)))
                     (v:translate new-x 0))
                   (draw-skill attack :occupied-left-p occupied-left-p
                                      :occupied-right-p occupied-right-p)))))))

(defclass drawing-state ()
  ((%occupied-corners :accessor occupied-corners
                      :initarg :occupied-corners))
  (:default-initargs :occupied-corners '()))

(defun occupied-corner-p (state corner)
  (let ((occupied-corners (occupied-corners state)))
    (member corner occupied-corners)))

(defun occupied-left-p (state direction)
  (occupied-corner-p state (case direction
                             (:q :aq) (:w :qw) (:e :we)
                             (:d :ed) (:s :ds) (:a :sa))))

(defun occupied-right-p (state direction)
  (occupied-corner-p state (case direction
                             (:q :qw) (:w :we) (:e :ed)
                             (:d :ds) (:s :sa) (:a :aq))))

(defparameter *save-path* #p"/tmp/vecto.png")

;; TODO make this dependent on tile type
;; TODO get rid of counter-clockwise diagonals in COMMON
(defun draw-tile (tile &key
                         (height 800)
                         (background-color '(0.5 0.5 0.5))
                         (save-path *save-path*)
                         bg-image (bg-x-offset 0) (bg-y-offset 0))
  (let ((state (make-instance 'drawing-state))
        (remaining-skills (copy-list (nt:skills tile))))
    (flet ((fetch-skills-if (predicate)
             (multiple-value-bind (skills remaining)
                 (φ:split predicate remaining-skills)
               (setf remaining-skills remaining)
               skills)))
      (macrolet ((process ((name) &body body)
                   (a:with-gensyms (skills)
                     `(a:when-let ((,skills (fetch-skills-if
                                             (lambda (,name) ,@body))))
                        (apply #'draw-skills state ,skills)))))
        (shapes:with-hex-tile (side height width)
            (:height height
             :background-color background-color
             :save-path save-path
             :bg-image bg-image
             :bg-x-offset bg-x-offset :bg-y-offset bg-y-offset)
          ;; Nets
          (process (x) (typep x 'nsk:net))
          ;; Attacks
          (dolist (direction ncom:*directions*)
            (process (x) (and (typep x 'na:attack)
                              (eq direction (nsk:direction x))))))
        ;; More drawing logic goes here
        )
      (when remaining-skills
        (dolist (skill remaining-skills)
          (warn "Skill remains at drawing completion: ~S" skill)
          (draw-skill skill))))))

(draw-tile (make-instance 'nervous-island.armies.hegemony:net-master))
(draw-tile (make-instance 'nervous-island.armies.hegemony:universal-soldier))
(draw-tile (make-instance 'nt:warrior :skills (list (na:melee :q 6))))
(let ((skills (list (na:melee :q) (na:ranged :q) (na:gauss-cannon :q))))
  (draw-tile (make-instance 'nt:warrior :skills skills)))
(let ((skills (list (na:melee :s) (nsk:net :a) (nsk:net :d))))
  (draw-tile (make-instance 'nt:warrior :skills skills)))
