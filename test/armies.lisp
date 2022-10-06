;;;; test/armies.lisp

(in-package #:nervous-island/test)

(defun make-all-armies ()
  ;; TODO remove the extra copy made in NI.USER
  (loop with prefix = (symbol-name '#:nervous-island.armies.)
        for package in (sort (copy-list (list-all-packages)) #'string<
                             :key #'package-name)
        for package-name = (package-name package)
        for result = (search prefix package-name)
        when (and result (= 0 result))
          collect (make-instance (find-symbol (symbol-name '#:army) package))))

(define-test armies-instantiate
  (dolist (army (make-all-armies))
    (let* ((elements (append (nr:hq-elements army)
                             (nr:elements army)
                             ;; TODO: tokens can and will conflict with tiles.
                             ;; (nr:tokens army)
                             )))
      (dolist (element (remove-duplicates elements :test #'eqv))
        (let ((name (class-name (class-of element)))
              (package (symbol-package (class-name (class-of army)))))
          (multiple-value-bind (value foundp)
              (find-symbol (symbol-name name) package)
            (true value)
            (is eqv :external foundp
                "~A not external in ~A" name (nel:name army))))))))
