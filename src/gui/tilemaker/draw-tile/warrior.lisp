(in-package #:nervous-island.gui.tilemaker)

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
      (shapes:with-hex-tile (side height width)
          (:height height
           :background-color background-color
           :save-path save-path
           :bg-image bg-image
           :bg-x-offset bg-x-offset :bg-y-offset bg-y-offset)
        (macrolet ((process ((name) &body body)
                     (a:with-gensyms (skills)
                       `(a:when-let ((,skills (fetch-skills-if
                                               (lambda (,name) ,@body))))
                          (apply #'draw-skills state ,skills)))))
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
