;;;; src/user.lisp

(in-package #:nervous-island.common)

(macrolet
    ((create-user-package ()
       (flet ((make-army-nicknames ()
                (loop with prefix = (symbol-name '#:nervous-island.armies.)
                      for package in (sort (copy-list (list-all-packages))
                                           #'string< :key #'package-name)
                      for package-name = (package-name package)
                      for result = (search prefix package-name)
                      when (and result (= 0 result))
                        collect (list (make-symbol (subseq package-name
                                                           (length prefix)))
                                      (make-symbol package-name)))))
         `(uiop:define-package #:nervous-island.user
            (:use #:nervous-island.cl
                  #:binding-arrows)
            (:local-nicknames (#:a #:alexandria)
                              (#:s #:split-sequence))
            ;; Tiles
            (:local-nicknames (#:ncom #:nervous-island.common)
                              (#:nel #:nervous-island.element)
                              (#:nsk #:nervous-island.skill)
                              (#:na #:nervous-island.attack)
                              (#:ne #:nervous-island.effect)
                              (#:nto #:nervous-island.token)
                              (#:nr #:nervous-island.army)
                              (#:nt #:nervous-island.tile))
            ;; State
            (:local-nicknames (#:nc #:nervous-island.coord)
                              (#:nsp #:nervous-island.space)
                              (#:nb #:nervous-island.board))
            ;; Old junk
            ;; (:local-nicknames (#:nd #:nervous-island.damage)
            ;;                   (#:nch #:nervous-island.choice)
            ;;                   (#:np #:nervous-island.player)
            ;;                   (#:nph #:nervous-island.phase)
            ;;                   (#:nst #:nervous-island.state))
            ;; Nervous Island armies
            (:local-nicknames ,@(make-army-nicknames))))))
  (create-user-package))

(in-package #:nervous-island.user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Describer

(defstruct (bulk (:constructor make-bulk (element))) element (count 0))

(defun elements-bulk (elements)
  (let ((vector (make-array 0 :adjustable t :fill-pointer t)))
    (dolist (element elements (coerce vector 'list))
      (let* ((class (class-of element))
             (bulk (or (find class vector
                             :key (alexandria:compose #'class-of #'bulk-element))
                       (let ((bulk (make-bulk element)))
                         (vector-push-extend bulk vector)
                         bulk))))
        (incf (bulk-count bulk))))))

(defgeneric children (x)
  (:method (x) '())
  (:method ((cons cons)) cons)
  (:method ((army nervous-island.army:army))
    (elements-bulk (append (nervous-island.army:hq-elements army)
                           (nervous-island.army:elements army)
                           (nervous-island.army:tokens army))))
  (:method ((bulk bulk))
    (let ((element (bulk-element bulk)))
      (typecase element
        (nervous-island.skill:skill-having
         (nervous-island.common:set-contents
          (nervous-island.skill:skills element)))
        (t '())))))

(defun describe-army (army &optional (stream *standard-output*))
  (flet ((frob (stream depth thing)
           (declare (ignore depth))
           (typecase thing
             (bulk (format stream "~A x~D"
                           (bulk-element thing) (bulk-count thing)))
             (cons (princ "All armies:" stream))
             (t (princ thing stream)))))
    (fresh-line)
    (terpri)
    (utilities.print-tree:print-tree
     stream army
     (utilities.print-tree:make-node-printer #'frob nil #'children))))

(defun frob-faq (pathname prefix &aux (i 0))
  (flet ((frob (x)
           (let ((name (a:symbolicate prefix '#:. (format nil "~D" (incf i)))))
             `(define-entry ,name ()
                :polish ,x :english "TODO"))))
    (let* ((file (a:read-file-into-string pathname))
           (lines (s:split-sequence #\Newline file :remove-empty-subseqs t))
           (forms (mapcar #'frob lines))
           (*print-pprint-dispatch* (copy-pprint-dispatch))
           (*print-case* :downcase))
      (set-pprint-dispatch 'null (lambda (stream object)
                                   (declare (ignore object))
                                   (princ "()" stream)))
      (format t "~&~{~S~^~%~%~}" forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML generator

(defparameter *css* "
.hex {
  clip-path: polygon(25% 5%, 75% 5%, 100% 50%, 75% 95%, 25% 95%, 0% 50%);
  position: absolute;
}
")

(defun call-with-html-base (thunk)
  (spinneret:with-html
    (:doctype)
    (:html
     (:head (:title "Nervous Island board")
            (:style *css*))
     (:body (:div :class "board" (funcall thunk))))))

(defun make-hex-css-style (scale top left background)
  (format nil "width: ~Dpx; height: ~:*~Dpx; ~
               top: ~Dpx; left: ~Dpx; background: ~A;"
          scale top left background))

(defun html-generate-board (&key (board (nb:standard-board)) (scale 200))
  (multiple-value-bind (dimensions max min) (nb:dimensions board)
    (declare (ignore max))
    (destructuring-bind (height width) dimensions
      (dotimes (y height)
        (dotimes (x width)
          (let* ((nx (- x (first min)))
                 (ny (- y (second min)))
                 (q nx)
                 (r (- ny (/ (- q (logand q 1)) 2)))
                 (top (* (+ (if (oddp x) (* scale 0.5) 0) (* y scale)) 0.95))
                 (left (* x scale 0.8))
                 (axial (nc:axial q r))
                 (background (if (nb:find-space board axial)
                                 "#CCCCCC"
                                 "#FFFFFF"))
                 (id (format nil "axial.~D.~D" q r)))
            (spinneret:with-html
              (:div :class "hex" :id id
                    :style (make-hex-css-style scale top left background)
                    (:h3 :style "text-align: center;"
                         (format nil "~D ~D" q r))))
            (a:when-let* ((space (nb:find-space board axial))
                          (unit (nsp:unit space)))
              (spinneret:with-html
                (:div :class "hex"
                      :style (make-hex-css-style (* scale 0.6)
                                                 (+ top (* scale 0.2))
                                                 (+ left (* scale 0.2))
                                                 "#CC0000")
                      (:h3 :style "text-align: center;"
                           (format nil "UNIT")))))))))))

(defparameter *board*
  (flet ((place (x y unit rot) (nsp:space (nc:axial x y) (list unit rot))))
    (-<> (nb:standard-board)
      (nb:augment-board <> (place 0 0 'borgo:mutant 0))
      (nb:augment-board <> (place 0 1 'borgo:hq 1))
      (nb:augment-board <> (place 1 0 'outpost:hq 3))
      (nb:augment-board <> (place 2 -2 'outpost:runner 4)))))

(defun generate-board ()
  (a:with-output-to-file (spinneret:*html* #p"/tmp/a.html" :if-exists :supersede)
    (let* ((board *board*))
      (call-with-html-base (lambda () (html-generate-board :board board))))))
