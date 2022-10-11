;;;; test/armies.lisp

(in-package #:nervous-island/test)

(define-test armies-instantiate
  (dolist (army (nr:make-all-armies))
    ;; Elements should have their names exported from their army packages
    (let* ((package (symbol-package (class-name (class-of army))))
           (elements (append (nr:hq-elements army) (nr:elements army))))
      (dolist (element (remove-duplicates elements :test #'eqv))
        (let ((name (class-name (class-of element))))
          (multiple-value-bind (value foundp)
              (find-symbol (symbol-name name) package)
            (true value)
            (is eqv :external foundp
                "Element ~A not external in ~A" name (nel:name army))))))
    ;; Tokens should have their names exported from NI.TOKEN
    (let ((expected-package (find-package '#:nto)))
      (dolist (token (nr:tokens army))
        (let* ((name (class-name (class-of token)))
               (actual-package (symbol-package name)))
          (is eqv expected-package actual-package)
          (multiple-value-bind (value foundp)
              (find-symbol (symbol-name name) actual-package)
            (true value)
            (is eqv :external foundp
                "Token ~A not external in ~A" name expected-package)))))))
