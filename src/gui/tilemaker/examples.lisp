(in-package #:nervous-island.gui.tilemaker)

(draw-tile (make-instance 'nervous-island.armies.outpost:mobile-armor))
(draw-tile (make-instance 'nervous-island.armies.hegemony:gladiator))
(draw-tile (make-instance 'nervous-island.armies.moloch:clown))
(draw-tile (make-instance 'nervous-island.armies.moloch:armored-hunter))
(draw-tile (make-instance 'nervous-island.armies.borgo:net-fighter))
(draw-tile (make-instance 'nervous-island.armies.hegemony:guard))
(draw-tile (make-instance 'nervous-island.armies.hegemony:net-master))
(draw-tile (make-instance 'nervous-island.armies.hegemony:universal-soldier))
(draw-tile (make-instance 'nt:warrior :skills (list (na:melee :q 6))))
(let ((skills (list (na:melee :q) (na:ranged :q) (na:gauss-cannon :q))))
  (draw-tile (make-instance 'nt:warrior :skills skills)))
(let ((skills (list (na:melee :s) (nsk:net :a) (nsk:net :d))))
  (draw-tile (make-instance 'nt:warrior :skills skills)))

(draw-tile (make-instance 'nervous-island.armies.hegemony:boss))

(defparameter *armies*
  (alexandria:alist-hash-table
   '((:borgo 0.0 0.6 0.8)
     (:moloch 0.8 0.0 0.0)
     (:outpost 0.0 0.9 0.2)
     (:hegemony 0.8 0.7 0.0))))

(defun keyword-army (keyword)
  (let* ((prefix '#:nervous-island.armies)
         (package-name (format nil "~A.~A" prefix keyword))
         (package (uiop:find-package* package-name))
         (symbol-name (symbol-name '#:army))
         (symbol (find-symbol symbol-name package)))
    (make-instance symbol)))

(defun save-warriors (&optional (printp t))
  (let* ((army-names (a:hash-table-keys *armies*))
         (directory #p"/tmp/ni-warriors/"))
    (ensure-directories-exist directory)
    (loop
      with done = '()
      for army-name in army-names
      for army = (keyword-army army-name)
      for color = (gethash army-name *armies*)
      do (dolist (tile (nervous-island.army:elements army))
           (when (typep tile 'nt:warrior)
             (let* ((symbol (class-name (class-of tile)))
                    (package (package-name (symbol-package symbol)))
                    (string (format nil "~(~A.~A~)" package symbol)))
               (unless (member string done :test #'string=)
                 (push string done)
                 (when printp (format t "~A..." string))
                 (let ((pathname (merge-pathnames (make-pathname :name string
                                                                 :type "png")
                                                  directory)))
                   (draw-tile tile :save-path pathname
                                   :background-color color))
                 (when printp (format t " done.~%"))
                 (force-output))))))))
