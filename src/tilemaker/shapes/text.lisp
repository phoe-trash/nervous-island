(in-package #:nervous-island.tilemaker.shapes)

(defparameter *font-directory* #p"~/.local/share/fonts/")

(defun text (n &optional (side *side*))
  (v:with-graphics-state
    (v:translate 0 (* -0.065 side))
    (when (eql n 1) ;; 1 looks better when shifted slightly left
      (v:translate (* -0.01 side) 0))
    (v:set-rgba-fill 1 1 1 1)
    (let* ((pathname (merge-pathnames #p"SWZBLKN.TTF" *font-directory*))
           (font (v:get-font pathname)))
      (v:set-font font 90)
      (v:draw-centered-string 0 0 (format nil "~A" n)))))
