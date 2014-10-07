;;;;
;;;; ccl.lisp
;;;;
;;;; buildapp-sys code for Clozure CL.
;;;;

(in-package #:buildapp-sys)

(defvar *this-implementation* "ccl")

(define-condition compatibility-error (error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Saving executable cores is not available on ~A"
             *this-implementation*))))

(define-condition feature-compatibility-error (compatibility-error)
  ((feature
    :initarg :feature
    :reader feature-compatibility-error-feature))
  (:report
   (lambda (condition stream)
     (format stream "Feature ~A is not available on ~A"
             (feature-compatibility-error-feature condition)
             *this-implementation*))))

(defun quit (exit-code)
  (ccl:quit exit-code))

(defun check-buildapp-feature-compatibility (&optional feature)
  (case feature
    ((nil) t)
    (otherwise
     (error 'feature-compatibility-error
            :feature feature))))

(defun command-line-arguments ()
  ccl:*command-line-argument-list*)

(defun save-executable (output-file
                        &key entry-function compress core-only
                          &allow-other-keys)
  (when compress
    (error "Compression not supported on this CL implementation"))
  (let ((prepend-kernel-p (not core-only)))
    (ccl:save-application output-file
                          :toplevel-function entry-function
                          :prepend-kernel prepend-kernel-p))
  (probe-file output-file))

(defun native-namestring (pathname)
  (ccl:native-translated-namestring pathname))

(defun run-lisp-load (lisp-path load-file &key &allow-other-keys)
  (let ((process (ccl:run-program lisp-path
                                  (list "--no-init" ; no init file
                                        "--batch"
                                        "--quiet"
                                        "--load" (native-namestring load-file))
                                  :output *standard-output*)))
    (multiple-value-bind (status code)
        (ccl:external-process-status process)
      (unless (and (eql code 0)
                   (eql status :exited))
        (error "Process exited with status ~S and code ~S"
               status code)))
    t))

(defun gc ()
  (ccl:gc))


(defvar *final-debugger-hook* *debugger-hook*)

(defun debugger-hook ()
  *debugger-hook*)

(defun (setf debugger-hook) (new-value)
  (setf *debugger-hook* new-value))

(defmacro with-buildapp-building-debugger (&body body)
  `(let ((*debugger-hook* 'dump-file-debugger))
     (progn ,@body)
     (unless (eql *debugger-hook* 'dump-file-debugger)
       (setf *final-debugger-hook* *debugger-hook*))))

(defun initialize-debugging-hooks ()
  (setf *debugger-hook* *final-debugger-hook*))

(defun backtrace-as-list ()
  ;; FIXME: Use supported external functionality instead
  (ccl::backtrace-as-list))
