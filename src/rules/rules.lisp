;;;; src/rules/rules.lisp

(uiop:define-package #:nervous-island.rules
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:i #:in-nomine)
                    (#:φ #:phoe-toolbox)
                    (#:s #:split-sequence))
  (:export #:entry #:name #:polish #:english #:tags
           #:symbol-entry #:entry-boundp #:entry-makunbound #:unbound-entry))

(in-package #:nervous-island.rules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Framework

(defun format-nil (arg) (format nil arg))

(define-class entry ()
  ((name :type symbol)
   (polish :type string :transform #'format-nil)
   (english :type string :transform #'format-nil)
   (tags :type (φ:list-of keyword))))

(defmethod print-object ((entry entry) stream)
  (print-unreadable-object (entry stream :type t :identity nil)
    (format stream "~S ~A" (name entry) (tags entry))))

(i:define-namespace entry
  :name-type symbol
  :value-type entry
  ;; TODO :warn-on-redefinition t
  :type-name nil)

(defmacro define-entry (name (&rest tags) &body keys)
  (a:with-gensyms (entry original-entry)
    `(let ((,entry (make-instance 'entry :name ',name :tags ',tags ,@keys)))
       (if (entry-boundp ',name)
           (let ((,original-entry (symbol-entry ',name)))
             (unless (eqv ,entry ,original-entry)
               (setf (symbol-entry ',name) ,entry)))
           (setf (symbol-entry ',name) ,entry))
       (export ',name)
       ',name)))

(defun split-entry-name (name)
  (let* ((parts (s:split-sequence #\. (symbol-name name))))
    (loop for part in parts
          when (parse-integer part :junk-allowed t)
            collect it
          else collect part)))

(defun id< (symbol-1 symbol-2)
  (loop for x in (split-entry-name symbol-1)
        for y in (split-entry-name symbol-2)
        unless (typecase x
                 (string (string= x y))
                 (integer (= x y)))
          return (typecase x
                   (string (string< x y))
                   (integer (< x y)))))

(defun list-all-entries ()
  (let* ((hash-table (i:namespace-binding-table (i:symbol-namespace 'entry)))
         (entries (a:hash-table-values hash-table)))
    (sort entries #'id< :key #'name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Markdown renderer

(defvar *language* 'english)

(defun render-entry (stream entry colon at)
  (declare (ignore colon at))
  (format stream "### ~A~%" (name entry))
  (format stream "* *~A:* ~{`~(~A~)`~^, ~}~%~%" (case *language*
                                                  (polish "Tagi")
                                                  (english "Tags"))
          (tags entry))
  (format stream "~A" (funcall *language* entry)))

(defun render-entries (&key
                         (entries (list-all-entries))
                         (stream t)
                         (language 'english))
  (let ((*language* language))
    (format stream "~{~/nervous-island.rules::render-entry/~%~^~%~}" entries)))
