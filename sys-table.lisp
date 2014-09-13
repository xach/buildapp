;;;; sys-table.lisp

(in-package #:buildapp)

(defparameter *sys-table*
  (let ((table (make-hash-table :test 'equal))
        (wild (make-pathname :directory '(:relative "implementation")
                             :name :wild
                             :type "lisp"
                             :defaults #. (or *load-truename*
                                              *compile-file-truename*
                                              *default-pathname-defaults*))))
    (dolist (file (directory wild))
      (setf (gethash (pathname-name file) table) (file-string file)))
    table))

(defun known-implementations ()
  (sort (loop for k being each hash-key of *sys-table*
              unless (equalp k "stub")
              collect k)
        #'string<))

(define-condition unknown-implementation-error (error)
  ((implementation
    :initarg :implementation
    :reader unknown-implementation-error-implementation))
  (:report
   (lambda (condition stream)
     (format stream "Unknown implementation ~S, ~
                     expected one of: ~{~S~^, ~}"
             (unknown-implementation-error-implementation condition)
             (known-implementations)))))

(defun implementation-sys-string (implementation)
  (let ((string (gethash implementation *sys-table*)))
    (unless string
      (error 'unknown-implementation-error
             :implementation implementation))
    string))


(defparameter *default-implementation-paths*
  '(("ccl" . "ccl")
    ("sbcl" . "sbcl")
    ("clisp" . "clisp")))

(defun default-implementation-path (implementation)
  (let ((entry (assoc implementation *default-implementation-paths*
                      :test 'equalp)))
    (unless entry
      (error 'unknown-implementation-error
             :implementation implementation))
    (cdr entry)))
