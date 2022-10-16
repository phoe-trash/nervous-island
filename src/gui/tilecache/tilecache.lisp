(uiop:define-package #:nervous-island.gui.tilecache
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:tm #:nervous-island.gui.tilemaker)
                    (#:nel #:nervous-island.element))
  (:export #:ensure-tile-drawn))

(in-package #:nervous-island.gui.tilecache)

(defun color-string (color)
  (check-type color nel:color)
  (flet ((frob (x) (format nil "~(~X~)" (round (* x 255)))))
    (apply #'concatenate 'string (mapcar #'frob color))))

(defun army-tile-pathname (tile)
  (let* ((class-name (class-name (class-of tile)))
         (package-name (package-name (symbol-package class-name)))
         (prefix (symbol-name '#:nervous-island.armies.))
         (match (search prefix package-name)))
    (when (and match (= 0 match))
      (let ((army-name (string-downcase (subseq package-name (length prefix))))
            (tile-name (string-downcase (symbol-name class-name)))
            (color (color-string (nel:color (nel:owner tile)))))
        (make-pathname :directory (list :relative "army" army-name color)
                       :name tile-name
                       :type "png")))))

(defvar *default-cache-pathname*
  (ensure-directories-exist #p"/tmp/nervous-island/tilecache/"))
(defvar *cache-counter* 0)
(defvar *cache* '())

(defun in-memory-cache-based-tile-pathname (tile)
  (macrolet ((place () `(a:assoc-value *cache* tile :test #'eqv)))
    (multiple-value-bind (value foundp) (place)
      (if foundp
          value
          (setf (place) (make-pathname
                         :directory (list :relative "in-memory-cache")
                         :name (format nil "~8,'0D" (incf *cache-counter*))
                         :type "png"))))))

(defun tile-pathname (tile)
  (or (army-tile-pathname tile)
      (in-memory-cache-based-tile-pathname tile)
      (error "BUG: unable to make a pathname for ~S." tile)))

(defun ensure-tile-drawn (tile &key
                                 (cache *default-cache-pathname*)
                                 (height 300))
  (let* ((pathname (merge-pathnames (tile-pathname tile) cache))
         (color (nel:color (nel:owner tile))))
    (ensure-directories-exist pathname)
    (unless (probe-file pathname)
      (tm:draw-tile tile :save-path pathname
                         :background color
                         :height height))
    pathname))
