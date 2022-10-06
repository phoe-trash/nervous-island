;;;; src/user.lisp

(uiop:define-package #:nervous-island.user
  (:use #:cl)
  ;; Nervous Island packages
  (:local-nicknames (#:na #:nervous-island.attack)
                    (#:nb #:nervous-island.board)
                    (#:nc #:nervous-island.coord)
                    (#:nd #:nervous-island.damage)
                    (#:nch #:nervous-island.choice)
                    (#:ncom #:nervous-island.common)
                    (#:ne #:nervous-island.effect)
                    (#:ni #:nervous-island.instant)
                    (#:np #:nervous-island.player)
                    (#:nph #:nervous-island.phase)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nsp #:nervous-island.space)
                    (#:nst #:nervous-island.state)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  ;; Nervous Island armies
  (:local-nicknames (#:moloch #:nervous-island.armies.moloch)
                    (#:outpost #:nervous-island.armies.outpost)
                    (#:borgo #:nervous-island.armies.borgo)
                    (#:hegemony #:nervous-island.armies.hegemony)))

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
        (nervous-island.tile:skill-having
         (nervous-island.common:set-contents
          (nervous-island.tile:skills element)))
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

(defun make-all-armies ()
  (loop with prefix = (symbol-name '#:nervous-island.armies.)
        for package in (sort (copy-list (list-all-packages)) #'string<
                             :key #'package-name)
        for package-name = (package-name package)
        for result = (search prefix package-name)
        when (and result (= 0 result))
          collect (make-instance (find-symbol (symbol-name '#:army) package))))
