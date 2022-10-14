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
