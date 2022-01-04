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
   (%allocated-corners :accessor allocated-corners
                       :initarg :allocated-corners))
  (:default-initargs :allocated-corners '()
                     :tile (a:required-argument :tile)))

(defun allocated-left-p (state direction)
  (allocated-corner-p state (case direction
                              (:q :aq) (:w :qw) (:e :we)
                              (:d :ed) (:s :ds) (:a :sa))))

(defun allocated-right-p (state direction)
  (allocated-corner-p state (case direction
                              (:q :qw) (:w :we) (:e :ed)
                              (:d :ds) (:s :sa) (:a :aq))))

(defun compute-best-corners (state)
  (let* ((all-corners (copy-list ncom:*diagonals*))
         (directions '())
         (result (mapcar (a:rcurry #'cons 2) all-corners)))
    ;; Collect all directions
    (loop with skills = (nt:skills (tile state))
          with predicate = (a:rcurry #'typep 'nsk:directed)
          for directed in (remove-if-not predicate skills)
          do (pushnew (nsk:direction directed) directions))
    ;; Rank corners
    (loop for direction in directions
          for left-corner = (case direction
                              (:q :aq) (:w :qw) (:e :we)
                              (:d :ed) (:s :ds) (:a :sa))
          for right-corner = (case direction
                               (:q :qw) (:w :we) (:e :ed)
                               (:d :ds) (:s :sa) (:a :aq))
          do (decf (a:assoc-value result left-corner))
             (decf (a:assoc-value result right-corner)))
    (mapcar #'car (sort result #'> :key #'cdr))))

(defun allocate-undirected-skills (state skills)
  (let ((corners (compute-best-corners state)))
    (when (> (length skills) (length corners))
      ;; TODO handle running out of corners in a better way
      (error "Too many skills to allocate: ~S" skills))
    (loop for skill in skills
          for corner in corners
          do (push (cons skill corner) (allocated-corners state)))))

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

(defmethod draw-skill :around ((skill nsk:undirected) &rest args &key corner)
  (let* ((rotation (position corner ncom:*diagonals*)))
    (v:with-graphics-state
      (v:rotate (* rotation pi -1/3))
      (shapes:ability-circle)
      (apply #'call-next-method skill :rotation rotation args))))

(defmethod draw-skill ((initiative nsk:initiative) &key rotation)
  (shapes:text (nsk:value initiative) rotation))

(defmethod draw-skill ((initiative nsk:mobility) &key rotation)
  (shapes:text "m" rotation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRAW-SKILLS

(defgeneric draw-skills (state skill &rest skills))

(defmethod draw-skills (state (skill nsk:undirected) &rest skills)
  (dolist (skill (cons skill skills))
    (let ((corner (a:assoc-value (allocated-corners state) skill)))
      (draw-skill skill :corner corner))))

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
           (allocated-left-p (allocated-left-p state direction))
           (allocated-right-p (allocated-right-p state direction))
           (margin (* 0.1 shapes:*side*))
           (initial-offset (* 0.25 shapes:*side*))
           (begin (- initial-offset (if allocated-right-p margin 0)))
           (end (+ (- initial-offset) (if allocated-left-p margin 0)))
           (count (1+ (length attacks))))
      (loop for i from 0 below count
            for attack in attacks
            do (let* ((direction (nsk:direction attack))
                      (rotation (1- (position direction ncom:*directions*))))
                 (v:with-graphics-state
                   (v:rotate (* rotation pi -1/3))
                   (let ((new-x (a:lerp (/ (1+ i) count) begin end)))
                     (v:translate new-x 0))
                   (draw-skill attack
                               :allocated-left-p allocated-left-p
                               :allocated-right-p allocated-right-p)))))))

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
    (flet ((fetch-skills-if (predicate &key (removep t))
             (multiple-value-bind (skills remaining)
                 (φ:split predicate remaining-skills)
               (when removep (setf remaining-skills remaining))
               skills)))
      (let ((undirected (fetch-skills-if (a:rcurry #'typep 'nsk:undirected)
                                         :removep nil)))
        (allocate-undirected-skills state undirected))
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
          ;; Draw undirected skills here
          (process (x) (typep x 'nsk:undirected))
          ;; More drawing logic goes here
          ))
      (when remaining-skills
        (dolist (skill remaining-skills)
          (warn 'remaining-skill-after-drawing :skill skill)
          (draw-skill skill))))))


