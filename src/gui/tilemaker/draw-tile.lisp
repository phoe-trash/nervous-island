;;;; src/gui/tilemaker/draw-tile.lisp

(in-package #:nervous-island.gui.tilemaker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Protocol

(defparameter *default-save-path* #p"/tmp/vecto.png")

(defparameter *draw-tile-defaults*
  (list :height 800
        :background-color '(0.5 0.5 0.5)
        :save-path *default-save-path*
        :bg-x-offset 0
        :bg-y-offset 0))

(defgeneric draw-tile (tile &key height background-color save-path bg-image
                              bg-x-offset bg-y-offset &allow-other-keys)
  (:method :around (tile &rest args)
    (apply #'call-next-method tile (append args *draw-tile-defaults*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module

(defun partition-by-class (skills)
  (loop with hash-table = (make-hash-table)
        for skill in skills
        for class = (class-of skill)
        do (push skill (gethash class hash-table))
        finally (return (a:hash-table-values hash-table))))

(defun reconcile-effect-ranges (state clusters)
  (flet ((cluster-ranges (cluster) (mapcar #'nsk:direction cluster)))
    (let ((ranges-list (mapcar #'cluster-ranges clusters)))
      (loop with (ranges-1 . rest) = ranges-list
            for ranges-2 in rest
            unless (a:set-equal ranges-1 ranges-2)
              do (error "Unable to reconcile ranges in ~S" clusters)
            finally (setf (module-range-directions state) ranges-1)))))

(defun check-directed-undirected-effects (skills)
  (when (and (find-if (a:rcurry #'typep 'ne:directed-effect) skills)
             (find-if (a:rcurry #'typep 'ne:undirected-effect) skills))
    (error "Unable to draw a module with both directed and ~
            undirected effects: ~S" skills)))

(defun draw-module-background (state)
  (let ((range-directions (module-range-directions state)))
    (shapes:module-background)
    (dolist (direction range-directions)
      (let ((rotation (position direction ncom:*directions*)))
        (shapes:module-range-shadow rotation)))
    (shapes:module-ring)
    (dolist (direction range-directions)
      (let ((rotation (position direction ncom:*directions*)))
        (shapes:module-range rotation)))
    (shapes:module-circle)))

(defmethod draw-tile ((tile nt:module) &key height background-color save-path)
  (let ((state (make-instance 'drawing-state :tile tile))
        (skills (copy-list (vs:set-contents (nsk:skills tile)))))
    (let* ((predicate (a:rcurry #'typep '(and nsk:undirected (not ne:effect))))
           (undirected (φ:split predicate skills)))
      (allocate-undirected-skills state undirected))
    (check-directed-undirected-effects skills)
    (let* ((predicate (a:rcurry #'typep 'ne:directed-effect))
           (directed (φ:split predicate skills))
           (clusters (partition-by-class directed)))
      (reconcile-effect-ranges state clusters))
    (shapes:with-hex-tile (side height width)
        (:height height
         :background-color background-color
         :save-path save-path)
      (draw-module-background state)
      (flet ((process (predicate)
               (multiple-value-bind (good bad) (φ:split predicate skills)
                 (setf skills bad)
                 (when good (apply #'draw-skills state good)))))
        (process (lambda (x) (typep x '(and nsk:undirected (not ne:effect)))))
        (process (lambda (x) (typep x 'ne:effect))))
      (when skills
        (dolist (skill skills)
          (warn 'remaining-skill-after-drawing :skill skill)
          (draw-skill skill))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warrior

(defmethod draw-tile ((tile nt:warrior)
                      &key height background-color save-path
                        bg-image bg-x-offset bg-y-offset)
  (let ((state (make-instance 'drawing-state :tile tile))
        (skills (copy-list (vs:set-contents (nsk:skills tile)))))
    (let* ((predicate (a:rcurry #'typep '(and nsk:undirected (not ne:effect))))
           (undirected (φ:split predicate skills)))
      (allocate-undirected-skills state undirected))
    (shapes:with-hex-tile (side height width)
        (:height height
         :background-color background-color
         :save-path save-path
         :bg-image bg-image
         :bg-x-offset bg-x-offset :bg-y-offset bg-y-offset)
      (flet ((process (predicate)
               (multiple-value-bind (good bad) (φ:split predicate skills)
                 (setf skills bad)
                 (when good (apply #'draw-skills state good)))))
        (process (lambda (x) (typep x 'nsk:net)))
        (dolist (direction ncom:*directions*)
          (process (lambda (x) (and (typep x 'na:attack)
                                    (eq direction (nsk:direction x))))))
        (process (lambda (x) (typep x 'nsk:armor)))
        (process (lambda (x) (typep x 'nsk:undirected))))
      (when skills
        (dolist (skill skills)
          (warn 'remaining-skill-after-drawing :skill skill)
          (draw-skill skill))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instant

(defmethod draw-tile ((tile nt:instant)
                      &key height background-color save-path
                        bg-image bg-x-offset bg-y-offset)
  (shapes:with-hex-tile (side height width)
      (:height height
       :background-color background-color
       :save-path save-path
       :bg-image bg-image
       :bg-x-offset bg-x-offset :bg-y-offset bg-y-offset
       :instantp t)
    (draw-instant tile background-color)))

(defgeneric draw-instant (tile background-color &rest rest))

(defmethod draw-instant :around ((tile nt:instant) background-color &key)
  (v:with-graphics-state
    (shapes::hexagon (* 0.96 shapes:*side*))
    (v:set-rgb-stroke 0.95 0.95 0.85)
    (v:set-line-width (* 0.04 shapes:*side*))
    (v:with-graphics-state
      (call-next-method)
      (v:with-graphics-state
        (v:even-odd-fill-and-stroke))
      (v:with-graphics-state
        (v:even-odd-fill))))
  (v:with-graphics-state
    (shapes::hexagon (* 0.95 shapes:*side*))
    (shapes::hexagon shapes:*side*)
    (v:even-odd-fill)))

(defmethod draw-instant ((tile nt:battle) background-color
                         &key (side shapes:*side*))
  (flet ((kaboom (spikes scale &optional (diff 1/2))
           (v:move-to (* side scale (- 1 diff) (sin 0))
                      (* side scale (- 1 diff) (cos 0)))
           (dotimes (i spikes)
             (let ((mult (if (oddp i) 1 (- 1 diff))))
               (v:line-to (* side scale mult (sin (* i 2 pi (/ spikes))))
                          (* side scale mult (cos (* i 2 pi (/ spikes)))))))))
    (kaboom 40 0.7)
    (kaboom 16 0.2 0.4)))

(defmethod draw-instant ((tile nt:move) background-color
                         &key (side shapes:*side*))
  (shapes::%mobility (* 8 side) nil))

(defmethod draw-instant ((tile nt:push-back) background-color
                         &key (side shapes:*side*))
  (shapes::%push-back (* 8 side) nil))

(defmethod draw-instant ((tile nt:instant) background-color &key)
  (warn 'drawing-an-unknown-skill :skill tile))

(defmethod draw-instant ((tile nt:air-strike) background-color
                         &key (side shapes:*side*))
  (shapes::%bomb (* 8 side) nil))

(defmethod draw-instant ((tile nt:sniper) background-color
                         &key (side shapes:*side*))
  (shapes::%sniper (* 8 side) nil))

(defmethod draw-instant ((tile nt:grenade) background-color
                         &key (side shapes:*side*))
  (shapes::%grenade (* 8 side) nil))
