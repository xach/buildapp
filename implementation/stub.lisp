;;;;
;;;; <implementation>.lisp
;;;;
;;;; buildapp-sys code for <implementation>
;;;;

(in-package #:buildapp-sys)

(defun quit (exit-code)
  (declare (ignore exit-code))
  (error "Unimplemented"))

(defun check-buildapp-feature-compatibility (&optional feature)
  (declare (ignore feature))
  (error "Unimplemented"))

(defun command-line-arguments ()
  (error "Unimplemented"))

(defun save-executable (output-file
                        &key entry-function compress core-only
                          &allow-other-keys)
  (declare (ignore output-file entry-function compress core-only))
  (error "Unimplemented"))

(defun native-namestring (pathname)
  (declare (ignore pathname))
  (error "Unimplemented"))

(defun run-lisp (lisp-path load-file &key &allow-other-keys)
  (declare (ignore lisp-path load-file))
  (error "Unimplemented"))

(defun gc ()
  (error "Unimplemented"))


(defvar *final-debugger-hook* *debugger-hook*)

(defun dump-file-debugger (condition previous-hook)
    "The function to call if there are errors when loading the dump file."
    (declare (ignore previous-hook))
    (format *system-load-output* "~&Fatal ~A:~%  ~A~%"
            (type-of condition) condition)
    (print (macroexpand-1 '(backtrace-as-list)) *logfile-output*)
    (macroexpand-1 '(quit 111)))

(defmacro with-buildapp-building-debugger (&body body)
  `(let ((*debugger-hook* 'dump-file-debugger))
     (progn ,@body)
     (unless (eql *debugger-hook* 'dump-file-debugger)
       (setf *final-debugger-hook* *debugger-hook*))))

(defun initialize-debugging-hooks ()
  (setf *debugger-hook* *final-debugger-hook*))

(defun backtrace-as-list ()
  (error "Unimplemented"))
