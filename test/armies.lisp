;;;; test/armies.lisp

(in-package #:nervous-island/test)

(define-test armies-instantiate
  (let* ((packages (loop with prefix = (symbol-name '#:nervous-island.armies.)
                         for package in (list-all-packages)
                         for package-name = (package-name package)
                         for result = (search prefix package-name)
                         when (and result (= 0 result))
                           collect package)))
    (dolist (package packages)
      (let ((symbol (find-symbol (symbol-name '#:army) package)))
        (true (make-instance symbol))))))
