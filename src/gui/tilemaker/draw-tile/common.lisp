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
