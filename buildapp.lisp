;;;;
;;;; Copyright (c) 2010 Zachary Beane, All Rights Reserved
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

;;;; buildapp.lisp

(in-package #:buildapp)

(defparameter *short-usage*
  "Usage: buildapp --output OUTPUT-FILE [--flag1 value1 ...]

For more usage info, try `buildapp --help'
")

(defparameter *usage*
  "Usage: buildapp --output OUTPUT-FILE [--flag1 value1 ...]

Required flags:
  --output OUTPUT-FILE      Use OUTPUT-FILE as the name of the executable
                              to create

Toplevel flag:
  --entry NAME              Use the function identified by NAME as the
                              executable's toplevel function. Called
                              with SB-EXT:*POSIX-ARGV* as its only
                              argument. If NAME has a colon, it is
                              treated as a package separator,
                              otherwise CL-USER is the implied
                              package.

Action flags:
  --load FILE               Load FILE. CL:*PACKAGE* is bound to the CL-USER
                              package before loading
  --load-system NAME        Load an ASDF system identified by NAME
  --require NAME            Use CL:REQUIRE to load NAME
  --eval CODE               Use CL:EVAL to evaulate CODE. The code is read
                              with CL:READ-FROM-STRING in the CL-USER package

There may be any number of load/load-system/require/eval flags. Each
is executed in command-line order before creating an executable.

Load path flags:
  --load-path DIRECTORY     When handling a --load, search DIRECTORY for
                              files to load
  --asdf-path DIRECTORY     When handling a --load-system, search DIRECTORY
                              for ASDF system files to load
  --asdf-tree DIRECTORY     When handling a --load-system, search DIRECTORY
                              and all its subdirectories for ASDF system
                              files to load

There may be any number of load-path/asdf-path/asdf-tree
flags. asdf-path arguments take precedence over asdf-tree arguments.

Other flags:
  --help                    Show this usage message
  --logfile FILE            Log compilation and load output to FILE
  --sbcl PATH-TO-SBCL       Use PATH-TO-SBCL instead of the sbcl program
                              found in your PATH environment variable

For the latest documentation, see http://www.xach.com/lisp/buildapp/
")

(define-condition silent-exit-error (error) ())

(defparameter *system-load-output* *standard-output*)
(defparameter *logfile-output* (make-broadcast-stream))

(dumpable check-pseudosymbol
  (defun check-pseudosymbol (package-name symbol-name pretty-name)
    (let ((package (find-package package-name)))
      (unless package
        (error "Entry function ~A package ~A not found" pretty-name package-name))
      (let ((symbol (find-symbol symbol-name package)))
        (unless symbol
          (error "Entry function ~A not found in package ~A" pretty-name package-name))
        (unless (fboundp symbol)
          (error "Entry function ~A is not fbound" pretty-name))))))

(defun pseudosymbol-check-form (pseudosymbol)
  `(check-pseudosymbol ,(string-upcase (package-string pseudosymbol))
                       ,(string-upcase (symbol-string pseudosymbol))
                       ,(princ-to-string pseudosymbol)))

(dumpable debugger
  (defun dump-file-debugger (condition previous-hook)
    "The function to call if there are errors when loading the dump file."
    (declare (ignore previous-hook))
    (format *system-load-output* "~&Fatal ~A:~%  ~A~%"
            (type-of condition) condition)
    (print (sb-debug:backtrace-as-list) *logfile-output*)
    (sb-ext:quit :unix-status 111)))

(defun command-line-debugger (condition previous-hook)
  "The function to call if there are errors in the command-line
buildapp application."
  (declare (ignore previous-hook))
  (unless (typep condition 'silent-exit-error)
    (format *error-output*
            "~&Fatal ~A: ~%  ~A~%" (type-of condition) condition)
    (when (typep condition 'command-line-error)
      (terpri *error-output*)
      (write-string *short-usage* *error-output*)))
  (print (sb-debug:backtrace-as-list) *logfile-output*)
  (sb-ext:quit :unix-status 1))

(dumpable asdf-ops
  (progn
    (defparameter *load-system* nil)
    (defparameter *traversal-parent* nil)
    (defparameter *traversal-parents* nil)

    (defmethod asdf::traverse :around ((operation asdf:load-op)
                                       (system asdf:system))
      "Gather some relationship information about systems."
      (if *load-system*
          (progn
            (when *traversal-parent*
              (push *traversal-parent* (gethash system *traversal-parents* nil)))
            (let ((*traversal-parent* system))
              (call-next-method)))
          (call-next-method)))

    (defmethod asdf:perform :around ((operation asdf:load-op)
                                     (system asdf:system))
      "Display terse information about loading systems."
      (if *load-system*
          (let ((parents (gethash system *traversal-parents*)))
            (format *system-load-output*
                    ";; loading system ~A ~@[(needed by ~{~A~^, ~})~]~%;;  from ~A~%"
                    (asdf:component-name system)
                    (mapcar #'asdf:component-name parents)
                    (asdf:component-pathname system))
            (force-output *system-load-output*)
            (call-next-method))
          (call-next-method)))

    (defmethod asdf:perform :around ((operation asdf:load-op)
                                     (component asdf:cl-source-file))
      "Try recompiling stale FASLs once before erroring. From the SBCL
manual."
      (handler-case (call-next-method operation component)
        (sb-ext:invalid-fasl ()
          (asdf:perform (make-instance 'asdf:compile-op) component)
          (call-next-method))))

    (defun remove-dumper-methods ()
      "Remove any methods added to ASDF GFs as part of the dump process."
      (flet ((zap-method (gf argtypes)
               (let ((method (find-method gf '(:around)
                                          (mapcar #'find-class argtypes))))
                 (when method
                   (remove-method gf method)))))
        (zap-method #'asdf:perform '(asdf:load-op asdf:cl-source-file))
        (zap-method #'asdf:perform '(asdf:load-op asdf:system))
        (zap-method #'asdf::traverse '(asdf:load-op asdf:system))))

    (defun load-system (name)
      "Load ASDF system identified by NAME."
      (let ((*standard-output* *logfile-output*)
            (*error-output* *logfile-output*)
            (*compile-verbose* t)
            (*compile-print* t)
            (*load-system* t)
            (*traversal-parents* (make-hash-table)))
        (handler-bind
            ((warning
              (lambda (condition)
                (when *compile-file-truename*
                  (unless (typep condition 'style-warning)
                    (error "Compilation failed: ~A in ~A"
                           condition
                           *compile-file-truename*))))))
          (asdf:oos 'asdf:load-op name)
          t)))
    ))

(dumpable file-ops
  (progn
    (defparameter *load-search-paths*
      (list *default-pathname-defaults*))

    (defun load-file (file)
      "Search for FILE in *LOAD-SEARCH-PATHS* and, if found, load
it. If an exact filename is not found, file.lisp is also tried."
      (dolist (path *load-search-paths*)
        ;; Try the exact name first, <name>.lisp second
        (let* ((p1 (merge-pathnames file path))
               (p2 (merge-pathnames (make-pathname :type "lisp"
                                                   :defaults file)
                                    path))
               (truename (or (probe-file p1) (probe-file p2))))
          (when truename
            (format *system-load-output* ";; loading file ~S~%" truename)
            (return-from load-file
              (let ((*standard-output* *logfile-output*)
                    (*package* (find-package '#:cl-user)))
                (load truename :verbose t :print t))))))
      (error "File ~S not found" file))))


(defun dumper-action-forms (dumper)
  "Return a list of forms to implement DUMPER's actions, i.e. the
--load, --load-system, --require, and --eval arguments."
  (loop for (type object) in (reverse (actions dumper))
        collect
        (ecase type
          (:eval
           `(eval (let ((*package* (find-package '#:cl-user)))
                    (read-from-string ,object))))
          (:load
           `(load-file ,object))
          (:load-system
           `(load-system ,object))
          (:require
           `(require ',(make-symbol (string-upcase object)))))))

(defun dumper-action-form (dumper)
  (let ((forms (dumper-action-forms dumper)))
    (if (needs-asdf-p dumper)
        `(let ((asdf:*central-registry* (list* ,@(asdf-system-directories
                                                  dumper)
                                               asdf:*central-registry*)))
           ,@forms)
        `(progn ,@forms))))

(defun dumpfile-forms (dumper)
  "Return a list of forms to be saved to a dumpfile."
  (let* ((package (package dumper))
         (entry (and (entry dumper)
                     (make-pseudosymbol (entry dumper))))
         (asdf (needs-asdf-p dumper)))
    `((cl:defpackage ,package
        (:use #:cl))
      (cl:in-package ,package)
      (defparameter *system-load-output* *standard-output*)
      (defvar *logfile-output*)
      ,(dump-form 'debugger)
      (setf sb-ext:*invoke-debugger-hook* 'dump-file-debugger)
      ,@(if (logfile dumper)
            `((defparameter *logfile-output*
                (open ,(logfile dumper) :direction :output
                      :if-exists :supersede))
              (setf *system-load-output* (make-broadcast-stream
                                          *standard-output*
                                          *logfile-output*)))
            '((defparameter *logfile-output*
                (make-broadcast-stream))))
      ;; Check that S-L-A-D will work as needed
      (unless (find-symbol "SAVE-RUNTIME-OPTIONS" '#:sb-impl)
        (error "This SBCL, ~A, does not support :SAVE-RUNTIME-OPTIONS"
               (lisp-implementation-version)))
      ;; Check for writability to the output file
      (with-open-file (stream ,(output dumper)
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :append)
        (write-line "buildapp write check" stream))
      ,@(when asdf
              `((require '#:asdf)
                ,(dump-form 'asdf-ops)))
      ,(dump-form 'file-ops)
      ,@(mapcar (lambda (path)
                  `(push ,(directorize path) *load-search-paths))
                (load-paths dumper))
      ,(dumper-action-form dumper)
      ,@(when asdf
              '((remove-dumper-methods)))
      ,@(when entry
              (list
               (dump-form 'check-pseudosymbol)
               (pseudosymbol-check-form entry)))
      (ignore-errors (close *logfile-output*))
      ;; Remove buildapp artifacts from the system
      (in-package #:cl-user)
      (setf sb-ext:*invoke-debugger-hook* nil)
      (delete-package ',package)
      (sb-ext:gc :full t)
      (sb-ext:save-lisp-and-die
        ,(output dumper)
        :executable t
        :save-runtime-options t
        ,@(when entry
                (list :toplevel
                      `(lambda ()
                         (with-simple-restart (abort "Exit application")
                           (,entry
                            sb-ext:*posix-argv*)))))))))

(defun write-dumpfile (dumper stream)
  (let ((*print-case* :downcase))
    (dolist (form (dumpfile-forms dumper))
      (print form stream))))

(defun dump-to-file (dumper file)
  "Save the forms of DUMPER to FILE."
  (with-open-file (stream file :direction :output
                          :if-exists :supersede)
    (let ((*print-case* :downcase))
      (write-dumpfile dumper stream))))


(defun main (argv)
  "Create an executable from the command-line arguments provided in
ARGV. See *USAGE* for details."
  (when (string-equal (second argv) "--help")
    (write-string *usage* *standard-output*)
    (sb-ext:quit))
  (let ((dumper (command-line-dumper (rest argv)))
        (*package* (find-package :buildapp)))
    (with-tempfile (stream ("dumper.lisp" file))
      (write-dumpfile dumper stream)
      (force-output stream)
      (when (dumpfile-copy dumper)
        (copy-file file (dumpfile-copy dumper)))
      (let ((process
             (sb-ext:run-program (sbcl dumper)
                                 (list "--noinform"
                                       "--no-userinit"
                                       "--no-sysinit"
                                       "--load" (sb-ext:native-namestring
                                                 (probe-file file)))
                                 :output *standard-output*
                                 :search t)))
        (let ((status (sb-ext:process-exit-code process)))
          (if (zerop status)
              (probe-file (output dumper))
              (error 'silent-exit-error)))))))

(defun build-buildapp (&optional (executable "buildapp"))
  (let ((full-output (merge-pathnames executable)))
    (main (list "sbcl"
                "--asdf-path"
                (sb-ext:native-namestring
                 (asdf:system-relative-pathname :buildapp "./"))
                "--load-system" "buildapp"
                "--entry" "buildapp:main"
                "--output"
                (sb-ext:native-namestring full-output)))))



(defun buildapp-init ()
  (setf sb-ext:*invoke-debugger-hook* 'command-line-debugger))

(pushnew 'buildapp-init sb-ext:*init-hooks*)

