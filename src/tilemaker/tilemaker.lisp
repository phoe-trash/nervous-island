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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions

(define-condition drawing-an-unknown-skill (warning)
  ((%skill :reader drawing-an-unknown-skill-skill :initarg :skill))
  (:report (lambda (condition stream)
             (format stream "Don't know how to draw skill ~S."
                     (drawing-an-unknown-skill-skill condition)))))

(define-condition remaining-skill-after-drawing (warning)
  ((%skill :reader remaining-skill-after-drawing-skill :initarg :skill))
  (:report (lambda (condition stream)
             (format stream "Skill remains after completing drawing: ~S"
                     (remaining-skill-after-drawing-skill condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRAWING-STATE

(defclass drawing-state ()
  ((%tile :accessor tile
          :initarg :tile)
   (%occupied-corners :accessor occupied-corners
                      :initarg :occupied-corners))
  (:default-initargs :occupied-corners '()
                     :tile (a:required-argument :tile)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRAW-SKILL

(defgeneric draw-skill (skill &key &allow-other-keys))

(defmethod draw-skill (skill &key)
  (warn 'drawing-an-unknown-skill :skill skill))

(defmethod draw-skill ((net nsk:net) &key)
  (let* ((direction (nsk:direction net))
         (rotation (1- (position direction ncom:*directions*))))
    (v:with-graphics-state
      (v:rotate (* rotation pi -1/3))
      (shapes:net))))

(defmethod draw-skill ((attack na:melee) &key) (shapes:melee))

(defmethod draw-skill ((attack na:ranged) &key) (shapes:ranged))

(defmethod draw-skill ((attack na:gauss-cannon) &key) (shapes:gauss))

(defmethod draw-skill ((initiative nsk:initiative) &key corner)
  (let* ((rotation (position corner ncom:*diagonals*)))
    (v:with-graphics-state
      (v:rotate (* rotation pi -1/3))
      (shapes:ability-circle)
      (shapes:initiative (nsk:value initiative) rotation)
      ;; TODO draw initiative here
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRAW-SKILLS

(defgeneric draw-skills (state skill &rest skills))

(defun compute-best-corners (state)
  (let* ((all-corners (copy-list ncom:*diagonals*))
         (skills (remove-if-not (a:rcurry #'typep 'nsk:directed)
                                (nt:skills (tile state)))))
    ;; TODO implement this
    (print skills)
    all-corners))

(defmethod draw-skills (state (skill nsk:initiative) &rest skills)
  (loop with corners = (compute-best-corners state)
        for corner in corners
        for skill in (cons skill skills)
        do (push corner (occupied-corners state))
           (draw-skill skill :corner corner)))

(defmethod draw-skills (state skill &rest skills)
  (declare (ignore state))
  (dolist (skill (cons skill skills))
    (draw-skill skill)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRAW-TILE

(defparameter *save-path* #p"/tmp/vecto.png")

(defgeneric draw-tile (tile &key
                              height background-color save-path bg-image
                              bg-x-offset bg-y-offset
                       &allow-other-keys))

(defparameter *draw-tile-defaults*
  (list :height 800
        :background-color '(0.5 0.5 0.5)
        :save-path *save-path*
        :bg-x-offset 0
        :bg-y-offset 0))

(defmethod draw-tile :around (tile &rest args)
  (apply #'call-next-method tile (append args *draw-tile-defaults*)))

;; TODO get rid of counter-clockwise diagonals in COMMON
(defmethod draw-tile ((tile nt:warrior)
                      &key height background-color save-path
                        bg-image bg-x-offset bg-y-offset)
  (let ((state (make-instance 'drawing-state :tile tile))
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
                              (eq direction (nsk:direction x)))))
          (process (x) (typep x 'nsk:initiative))
          ;; More drawing logic goes here
          ))
      (when remaining-skills
        (dolist (skill remaining-skills)
          (warn 'remaining-skill-after-drawing :skill skill)
          (draw-skill skill))))))


