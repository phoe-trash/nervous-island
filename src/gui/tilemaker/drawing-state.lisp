;;;; src/gui/tilemaker/drawing-state.lisp

(in-package #:nervous-island.gui.tilemaker)

(defclass drawing-state ()
  ((%tile :accessor tile
          :initarg :tile)
   (%allocated-corners :accessor allocated-corners
                       :initarg :allocated-corners)
   (%module-range-directions :accessor module-range-directions
                             :initarg :module-range-directions)
   (%hq-background-color :accessor hq-background-color
                         :initarg :hq-background-color))
  (:default-initargs :allocated-corners '()
                     :module-range-directions '()
                     :tile (a:required-argument :tile)
                     :hq-background-color '(0 0 0 1)))

(defun allocated-corner-p (state corner)
  (member corner (allocated-corners state) :key #'cdr))

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
    (loop with skills = (vs:set-contents (nsk:skills (tile state)))
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
  (flet ((preprocess (skills)
           (loop for skill in skills
                 if (typep skill 'nsk:toughness)
                   nconc (loop repeat (nsk:value skill)
                               collect skill)
                 else nconc (list skill))))
    (let* ((skills (preprocess skills))
           (corners (compute-best-corners state)))
      (when (> (length skills) (length corners))
        ;; TODO handle running out of corners in a better way
        (error "Too many skills to allocate: ~S" skills))
      (loop for skill in skills
            for corner in corners
            do (push (cons skill corner) (allocated-corners state))))))
