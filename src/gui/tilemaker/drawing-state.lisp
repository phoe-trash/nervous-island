(in-package #:nervous-island.gui.tilemaker)

(defclass drawing-state ()
  ((%tile :accessor tile
          :initarg :tile)
   (%allocated-corners :accessor allocated-corners
                       :initarg :allocated-corners))
  (:default-initargs :allocated-corners '()
                     :tile (a:required-argument :tile)))

(defun allocated-left-p (state direction)
  (allocated-corner-p state (case direction
                              (:q :aq) (:w :qw) (:e :we)
                              (:d :ed) (:s :ds) (:a :sa))))

(defun allocated-right-p (state direction)
  (allocated-corner-p state (case direction
                              (:q :qw) (:w :we) (:e :ed)
                              (:d :ds) (:s :sa) (:a :aq))))

(defun compute-best-corners (state)
  (let* ((all-corners (copy-list ncom:*diagonals*))
         (directions '())
         (result (mapcar (a:rcurry #'cons 2) all-corners)))
    ;; Collect all directions
    (loop with skills = (nt:skills (tile state))
          with predicate = (a:rcurry #'typep 'nsk:directed)
          for directed in (remove-if-not predicate skills)
          do (pushnew (nsk:direction directed) directions))
    ;; Rank corners
    (loop for direction in directions
          for left-corner = (case direction
                              (:q :aq) (:w :qw) (:e :we)
                              (:d :ed) (:s :ds) (:a :sa))
          for right-corner = (case direction
                               (:q :qw) (:w :we) (:e :ed)
                               (:d :ds) (:s :sa) (:a :aq))
          do (decf (a:assoc-value result left-corner))
             (decf (a:assoc-value result right-corner)))
    (mapcar #'car (sort result #'> :key #'cdr))))

(defun allocate-undirected-skills (state skills)
  (let ((corners (compute-best-corners state)))
    (when (> (length skills) (length corners))
      ;; TODO handle running out of corners in a better way
      (error "Too many skills to allocate: ~S" skills))
    (loop for skill in skills
          for corner in corners
          do (push (cons skill corner) (allocated-corners state)))))
