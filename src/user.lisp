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
            (:use #:nervous-island.cl)
            (:local-nicknames (#:a #:alexandria)
                              (#:s #:split-sequence)
                              (#:h #:hunchentoot)
                              (#:pt #:utilities.print-tree))
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
            ;; Tile drawing
            (:local-nicknames (#:ntc #:nervous-island.gui.tilecache))
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

(defstruct (bulk (:constructor %bulk (element))) element (count 0))

(defun bulk (elements)
  (let ((result '()))
    (dolist (element elements (nreverse result))
      (let* ((class (class-of element))
             (bulk (or (find class result
                             :key (a:compose #'class-of #'bulk-element))
                       (let ((bulk (%bulk element)))
                         (push bulk result)
                         bulk))))
        (incf (bulk-count bulk))))))

(defgeneric children (x)
  (:method (thing) '())
  (:method ((list list)) list)
  (:method ((quartermaster ne:quartermaster))
    (set-contents (ne:attack-types quartermaster)))
  (:method ((army nr:army))
    (bulk (append (nr:hq-elements army) (nr:elements army) (nr:tokens army))))
  (:method ((skill-having nsk:skill-having))
    (set-contents (nsk:skills skill-having)))
  (:method ((bulk bulk))
    (children (bulk-element bulk))))

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
    (pt:print-tree stream army
                   (pt:make-node-printer #'frob nil #'children))))

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
  display: flex;
  justify-content: center;
  align-items: center;
}
")

(defvar *root* #p"/tmp/nervous-island/")

(defun call-with-html-base (thunk)
  (spinneret:with-html
    (:doctype)
    (:html
     (:head (:title "Nervous Island board")
            (:style *css*)
            (:script :type "text/javascript"
                     :src "https://livejs.com/live.js"))
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
                                 "#F3F3F3"))
                 (id (format nil "axial.~D.~D" q r)))
            (spinneret:with-html
              (:div :class "hex" :id id
                    :style (make-hex-css-style scale top left background)
                    (:h3 :style "text-align: center;"
                         (format nil "~D ~D" q r))))
            (a:when-let* ((space (nb:find-space board axial))
                          (unit (nsp:unit space))
                          (unit-rotation (* (nsp:unit-rotation space) 60)))
              (let* ((cache (merge-pathnames #p"cache/" *root*))
                     (pathname (ntc:ensure-tile-drawn unit :height scale
                                                           :cache cache))
                     (namestring (enough-namestring
                                  (uiop:native-namestring pathname) *root*)))
                (spinneret:with-html
                  (:div :class "hex"
                        :style (make-hex-css-style scale top left "black")
                        (:img :src namestring
                              :style (format nil "transform: rotate(~Ddeg); ~
                                                  max-width: 100%; ~
                                                  max-height: 100%;"
                                             unit-rotation))))))))))))

(defparameter *board*
  (flet ((place (x y unit rot) (nsp:space (nc:axial x y) (list unit rot))))
    ;; TODO AUGMENT-SPACES, really necessary? there's a ton of things
    ;;      to think over here
    (nb:augment
     (nb:standard-board)
     (place 0 0 'borgo:mutant 0)
     (place 0 1 'borgo:hq 1)
     (place 1 0 'outpost:hq 3)
     (place 2 -2 'outpost:runner 4))))

(defun generate-board ()
  (let ((index (merge-pathnames #p"index.html" *root*)))
    (a:with-output-to-file (spinneret:*html* index :if-exists :supersede)
      (let* ((board *board*))
        (call-with-html-base (lambda () (html-generate-board :board board)))))))

#+(or)
(generate-board)

(defun make-acceptor (&optional (port 4242))
  (ensure-directories-exist *root*)
  (let ((acceptor (make-instance 'h:easy-acceptor
                                 :port port :document-root *root*)))
    (setf (h:acceptor-access-log-destination acceptor) nil
          (h:acceptor-message-log-destination acceptor) nil)
    acceptor))

(defvar *acceptor* (make-acceptor))
