;;;; src/user.lisp

(in-package #:nervous-island.common)

(macrolet
    ((create-user-package ()
       (flet ((make-army-nicknames ()
                (loop with prefix = (symbol-name '#:nervous-island.armies.)
                      for package in (sort (copy-list (list-all-packages))
                                           #'string<
                                           :key #'package-name)
                      for package-name = (package-name package)
                      for result = (search prefix package-name)
                      when (and result (= 0 result))
                        collect (list (make-symbol (subseq package-name
                                                           (length prefix)))
                                      (make-symbol package-name)))))
         `(uiop:define-package #:nervous-island.user
            (:use #:nervous-island.cl)
            (:local-nicknames (#:a #:alexandria)
                              (#:s #:split-sequence))
            ;; Nervous Island packages
            (:local-nicknames (#:ncom #:nervous-island.common)
                              (#:nel #:nervous-island.element)
                              (#:nsk #:nervous-island.skill)
                              (#:na #:nervous-island.attack)
                              (#:ne #:nervous-island.effect)
                              (#:nto #:nervous-island.token)
                              (#:nr #:nervous-island.army)
                              (#:nt #:nervous-island.tile))
            ;; Old junk
            ;; (:local-nicknames (#:nb #:nervous-island.board)
            ;;                   (#:nc #:nervous-island.coord)
            ;;                   (#:nd #:nervous-island.damage)
            ;;                   (#:nch #:nervous-island.choice)
            ;;                   (#:np #:nervous-island.player)
            ;;                   (#:nph #:nervous-island.phase)
            ;;                   (#:nsp #:nervous-island.space)
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
