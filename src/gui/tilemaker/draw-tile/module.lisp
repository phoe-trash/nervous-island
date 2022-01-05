(in-package #:nervous-island.gui.tilemaker)

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

(defmethod draw-tile ((tile nt:module) &key height background-color save-path)
  (let ((state (make-instance 'drawing-state :tile tile))
        (remaining-skills (copy-list (nt:skills tile))))
    (flet ((fetch-skills-if (predicate &key (removep t))
             (multiple-value-bind (skills remaining)
                 (φ:split predicate remaining-skills)
               (when removep (setf remaining-skills remaining))
               skills)))
      (let* ((predicate (a:rcurry #'typep 'ne:directed-effect))
             (directed (fetch-skills-if predicate :removep nil))
             (clusters (partition-by-class directed)))
        (reconcile-effect-ranges state clusters))
      (let* ((predicate (a:rcurry #'typep '(and nsk:undirected
                                            (not ne:effect))))
             (undirected (fetch-skills-if predicate :removep nil)))
        (allocate-undirected-skills state undirected))
      (shapes:with-hex-tile (side height width)
          (:height height
           :background-color background-color
           :save-path save-path)
        (let ((range-directions (module-range-directions state)))
          (shapes:module-background)
          (dolist (direction range-directions)
            (let ((rotation (position direction ncom:*directions*)))
              (shapes:module-range-shadow rotation)))
          (shapes:module-ring)
          (dolist (direction range-directions)
            (let ((rotation (position direction ncom:*directions*)))
              (shapes:module-range rotation)))
          (shapes:module-circle))
        (when remaining-skills
          (dolist (skill remaining-skills)
            (warn 'remaining-skill-after-drawing :skill skill)
            (draw-skill skill)))))))
