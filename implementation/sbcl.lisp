;;;;
;;;; sbcl.lisp
;;;;
;;;; buildapp-sys code for SBCL
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
  (sb-ext:exit :code exit-code))

(defun check-buildapp-feature-compatibility (&optional feature)
  (case feature
    ((nil)
     (unless (find-symbol "SAVE-RUNTIME-OPTIONS" '#:sb-impl)
       (error "This SBCL, ~A, does not support :SAVE-RUNTIME-OPTIONS"
              (lisp-implementation-version))))
    ((:compress-core)
     t)
    (otherwise
     (error 'feature-compatibility-error
            :feature feature))))

(defun command-line-arguments ()
  sb-ext:*posix-argv*)

(defun save-executable (output-file
                        &key entry-function compress core-only
                          &allow-other-keys)
  (sb-ext:save-lisp-and-die output-file
                            :save-runtime-options t
                            :toplevel entry-function
                            :executable (not core-only)
                            :compression compress))

(defun native-namestring (pathname)
  (sb-ext:native-namestring pathname))

(defun run-lisp-load (lisp-path load-file &key dynamic-space-size
                                            &allow-other-keys)
  (let* ((args (nconc
                (list "--noinform"
                      "--non-interactive"
                      "--no-userinit"
                      "--no-sysinit")
                (when dynamic-space-size
                  (list "--dynamic-space-size"
                        (princ-to-string dynamic-space-size)))
                (list "--load" (native-namestring load-file))))
         (process (sb-ext:run-program lisp-path args
                                      :output t
                                      :search t
                                      :wait t)))
    (let ((status (sb-ext:process-status process))
          (code (sb-ext:process-exit-code process)))
      (unless (and (eql code 0)
                   (eql status :exited))
        (error "Process exited with status ~S and code ~S"
               status code)))))

(defun gc ()
  (sb-ext:gc :full t))

(defvar *final-debugger-hook* *debugger-hook*)

(defmacro with-buildapp-building-debugger (&body body)
  `(let ((sb-ext:*invoke-debugger-hook* 'dump-file-debugger))
     (progn ,@body)
     (unless (eql sb-ext:*invoke-debugger-hook* 'dump-file-debugger)
       (setf *final-debugger-hook* sb-ext:*invoke-debugger-hook*))))

(defun initialize-debugging-hooks ()
  (setf sb-ext:*invoke-debugger-hook* *final-debugger-hook*))

(defun backtrace-as-list ()
  (sb-debug:backtrace-as-list))


