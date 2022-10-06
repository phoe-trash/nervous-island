;;;; test/armies.lisp

(in-package #:nervous-island/test)

(defun list-all-army-packages ()
  (loop with prefix = (symbol-name '#:nervous-island.armies.)
        for package in (list-all-packages)
        for package-name = (package-name package)
        for result = (search prefix package-name)
        when (and result (= 0 result))
          collect package))

(define-test armies-instantiate
  (dolist (package (list-all-army-packages))
    (let* ((symbol (find-symbol (symbol-name '#:army) package))
           (army (make-instance symbol))
           (elements (append (nr:hq-elements army)
                             (nr:elements army)
                             (nr:tokens army))))
      (dolist (element (remove-duplicates elements :test #'eqv))
        (let ((name (class-name (class-of element))))
          (multiple-value-bind (value foundp)
              (find-symbol (symbol-name name) package)
            (true value)
            (is eqv :external foundp
                "~A not external in ~A" name (nel:name army))))))))
