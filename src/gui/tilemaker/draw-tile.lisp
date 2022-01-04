(in-package #:nervous-island.gui.tilemaker)

(defparameter *default-save-path* #p"/tmp/vecto.png")

(defgeneric draw-tile (tile &key
                              height background-color save-path bg-image
                              bg-x-offset bg-y-offset
                       &allow-other-keys))

(defparameter *draw-tile-defaults*
  (list :height 800
        :background-color '(0.5 0.5 0.5)
        :save-path *default-save-path*
        :bg-x-offset 0
        :bg-y-offset 0))

(defmethod draw-tile :around (tile &rest args)
  (apply #'call-next-method tile (append args *draw-tile-defaults*)))

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
          (process (x) (typep x 'nsk:net))
          (dolist (direction ncom:*directions*)
            (process (x) (and (typep x 'na:attack)
                              (eq direction (nsk:direction x)))))
          (process (x) (typep x 'nsk:armor))
          (process (x) (typep x 'nsk:undirected))))
      (when remaining-skills
        (dolist (skill remaining-skills)
          (warn 'remaining-skill-after-drawing :skill skill)
          (draw-skill skill))))))
