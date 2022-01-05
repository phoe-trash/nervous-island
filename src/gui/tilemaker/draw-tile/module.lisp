(in-package #:nervous-island.gui.tilemaker)

(defmethod draw-tile ((tile nt:module) &key height background-color save-path)
  (let ((state (make-instance 'drawing-state :tile tile))
        (remaining-skills (copy-list (nt:skills tile))))
    (shapes:with-hex-tile (side height width)
        (:height height
         :background-color background-color
         :save-path save-path)
      (shapes:module-background)
      state
      (when remaining-skills
        (dolist (skill remaining-skills)
          (warn 'remaining-skill-after-drawing :skill skill)
          (draw-skill skill))))))
