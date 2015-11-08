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

(defparameter *output-type-pathname*
  #+sbcl
  (let* ((runtime-symbol (find-symbol "*RUNTIME-PATHNAME*" '#:sb-ext))
         (template (if runtime-symbol
                       (symbol-value runtime-symbol)
                       #p"buildapp")))
    (make-pathname :type (pathname-type template)))
  #+ccl
  (make-pathname :type #+windows "exe" #-windows nil)
  "This pathname is merged with the output parameter to produce the
  final output executable name. It's meant to automatically include
  the executable suffix .EXE on Windows.")

(defparameter *short-usage*
  "Usage: buildapp --output OUTPUT-FILE [--flag1 value1 ...]

For more usage info, try `buildapp --help'
")

(defparameter *usage*
  (concatenate 'string
    "Usage: buildapp --output OUTPUT-FILE [--flag1 value1 ...]

Required flags:
  --output OUTPUT-FILE      Use OUTPUT-FILE as the name of the executable
                              to create

Entry-point flags:
  --entry NAME              Use the function identified by NAME as the
                              executable's toplevel function. Called
                              with "
    #+sbcl "SB-EXT:*POSIX-ARGV*"
    #+ccl "(ccl::command-line-arguments)"
    " as its only
                              argument. If NAME has a colon, it is
                              treated as a package separator,
                              otherwise CL-USER is the implied
                              package.
  --dispatched-entry DNAME  Specify one possible entry function, depending
                              on the name of the file that is used to
                              start the application. The syntax of
                              DNAME is APPLICATION-NAME/ENTRY-NAME. If the
                              name used to start the executable matches
                              APPLICATION-NAME, use ENTRY-NAME as the
                              entry point. This can be used to choose
                              one of many possible entry points by
                              e.g. symlinking names to the application
                              executable. If APPLICATION-NAME is empty, the
                              specified ENTRY-NAME is used as a default
                              if no other application names match. There
                              may be any number of dispatched entry points,
                              but only one default.

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
  --manifest-file FILE      When handling a --load-system, read a list of
                              ASDF system file pathnames from FILE as
                              possible matching systems.

There may be any number of load-path/asdf-path/asdf-tree/manifest-file
flags. They take priority in command-line order.

Other flags:"
#+sbcl
"
  --compress-core           Compress the core or executable; requires
                              configuration support in SBCL"
"
  --core-only               Make a core file only, not an executable"
#+sbcl
"
  --dynamic-space-size MB   Pass a --dynamic-space-size option to SBCL
                              when building; value is megabytes"
"
  --help                    Show this usage message
  --logfile FILE            Log compilation and load output to FILE"
#+sbcl
"
  --sbcl PATH-TO-SBCL       Use PATH-TO-SBCL instead of the sbcl program
                              found in your PATH environment variable"
#+ccl
"
  --ccl PATH-TO-CCL         Use PATH-TO-CCL instead of the ccl program
                              found in your PATH environment variable"
"
For the latest documentation, see http://www.xach.com/lisp/buildapp/
"))

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
    (print (macroexpand-1 '(backtrace-as-list)) *logfile-output*)
    #+sbcl (sb-ext:exit :code 111)
    #+ccl (ccl:quit 111)))

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
  (print (backtrace-as-list) *logfile-output*)
  (quit 1))

(dumpable asdf-ops
  (progn
    (defun system-search-table (&rest pathnames)
      (let ((table (make-hash-table :test 'equalp)))
        (dolist (pathname pathnames table)
          (setf (gethash (pathname-name pathname) table)
                (probe-file pathname)))))

    (defvar *asdf-systems-table*)

    (defun system-search-function (name)
      (gethash name *asdf-systems-table*))

    (defun load-system (name)
      "Load ASDF system identified by NAME."
      (let ((*standard-output* *logfile-output*)
            (*error-output* *logfile-output*)
            (*compile-verbose* t)
            (*compile-print* t))
        (handler-bind
            ((warning
              (lambda (condition)
                (when *compile-file-truename*
                  (unless (typep condition 'style-warning)
                    (error "Compilation failed: ~A in ~A"
                           condition
                           *compile-file-truename*))))))
          (format *system-load-output* ";; loading system ~S~%" name)
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

(defun invoke-debugger-hook-wrapper (form)
  `(let ((previous-hook #+sbcl sb-ext:*invoke-debugger-hook*
                        #+ccl  ccl::*debugger-hook*)
         #+sbcl (sb-ext:*invoke-debugger-hook* sb-ext:*invoke-debugger-hook*)
         #+ccl  (ccl::*debugger-hook* ccl::*debugger-hook*))
     (progn ,form)
     #+sbcl
     (unless (eql sb-ext:*invoke-debugger-hook* previous-hook)
       (setf *post-invoke-debugger-hook*
             sb-ext:*invoke-debugger-hook*))
     #+ccl
     (unless (eql ccl::*debugger-hook* previous-hook)
       (setf *post-invoke-debugger-hook*
             ccl::*debugger-hook*))))

(defun dumper-action-form (dumper)
  (let ((forms (mapcar 'invoke-debugger-hook-wrapper
                       (dumper-action-forms dumper))))
    `(let ((*package* (find-package "CL-USER")))
       ,@forms)))

(defun dumpfile-forms (dumper)
  "Return a list of forms to be saved to a dumpfile."
  (let* ((package (package dumper))
         (output (merge-pathnames (output dumper) *output-type-pathname*))
         (entry-function-form
          (entry-function-form dumper))
         (asdf (needs-asdf-p dumper)))
    `((cl:defpackage ,package
        (:use #:cl))
      (cl:in-package ,package)
      (defparameter *post-invoke-debugger-hook* nil)
      (defparameter *system-load-output* *standard-output*)
      (defvar *logfile-output*)
      ,(dump-form 'debugger)
      (setf #+sbcl sb-ext:*invoke-debugger-hook*
            #+ccl  ccl::*debugger-hook*
            'dump-file-debugger)
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
      #+sbcl
      (unless (find-symbol "SAVE-RUNTIME-OPTIONS" '#:sb-impl)
        (error "This SBCL, ~A, does not support :SAVE-RUNTIME-OPTIONS"
               (lisp-implementation-version)))
      ,@(when (compress-core dumper)
              `((unless (member :sb-core-compression *features*)
                  (error "This SBCL does not support core compression"))))
      ;; Check for writability to the output file
      (with-open-file (stream ,(output dumper)
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :append)
        (write-line "buildapp write check" stream))
      (delete-file ,(output dumper))
      ,@(when asdf
              `((require '#:asdf)
                ,(dump-form 'asdf-ops)
                (setf *asdf-systems-table*
                      (system-search-table ,@(asdf-system-files dumper)))
                (push 'system-search-function
                      asdf:*system-definition-search-functions*)))
      ,(dump-form 'file-ops)
      ,@(mapcar (lambda (path)
                  `(push ,(directorize path) *load-search-paths*))
                (load-paths dumper))
      ,(dumper-action-form dumper)
      ,@(when entry-function-form
              (list (dump-form 'check-pseudosymbol)
                    (entry-function-check-form dumper)))
      (ignore-errors (close *logfile-output*))
      ;; Remove buildapp artifacts from the system
      (setf #+sbcl sb-ext:*invoke-debugger-hook*
            #+ccl  ccl::*debugger-hook*
            *post-invoke-debugger-hook*)
      ,@(when asdf
              '((setf asdf:*system-definition-search-functions*
                 (remove 'system-search-function
                   asdf:*system-definition-search-functions*))))
      (in-package #:cl-user)
      (delete-package ',package)
      #+sbcl (sb-ext:gc :full t)
      #+ccl  (ccl:gc)
      #+sbcl
      (sb-ext:save-lisp-and-die
       ,output
       ,@(unless (core-only dumper)
                 '(:executable t
                   :save-runtime-options t))
       ,@(when (compress-core dumper)
               '(:compression t))
       ,@(when entry-function-form
               (list :toplevel
                     entry-function-form)))
      #+ccl
      (ccl:save-application
       ,output
       ;; currently :native may not be supplied with :prepend-kernel
       ,@(unless (core-only dumper)
                 '(:prepend-kernel t))
       ,@(when entry-function-form
               (list :toplevel-function
                     entry-function-form))
       :purify t
       ;; The :application-class option must be supplied for the
       ;; :error-handler option to have any effect.  See
       ;; http://trac.clozure.com/ccl/ticket/1039.
       :application-class 'ccl::application
       :error-handler :quit))))

(defun write-dumpfile (dumper stream)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*package* (find-package '#:buildapp)))
      (dolist (form (dumpfile-forms dumper))
        (print form stream)))))

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
    (quit))
  (let* ((dumper (command-line-dumper (rest argv)))
         (*package* (find-package :buildapp))
         #+sbcl (dynamic-space-size (dynamic-space-size dumper)))
    (with-tempfile (stream ("dumper.lisp" file))
      (write-dumpfile dumper stream)
      (force-output stream)
      (when (dumpfile-copy dumper)
        (copy-file file (dumpfile-copy dumper)))
      (let ((process (run-program #+sbcl (sbcl dumper)
                                  #+ccl  (ccl  dumper)
                                  (flatten
                                   (list
                                    #+sbcl
                                    (when dynamic-space-size
                                      (list "--dynamic-space-size"
                                            (princ-to-string
                                             dynamic-space-size)))
                                    #+sbcl "--noinform"
                                    #+ccl  "--quiet"
                                    #+sbcl "--disable-debugger"
                                    #+sbcl "--no-userinit"
                                    #+sbcl "--no-sysinit"
                                    #+ccl  "--no-init"
                                    "--load" (native-namestring
                                              (probe-file file)))))))
        (if (zerop #+sbcl (sb-ext:process-exit-code process)
                   #+ccl  (ccl::external-process-%exit-code process))
            (probe-file (output dumper))
            (error 'silent-exit-error))))))

(defun build-buildapp (&optional (executable "buildapp"))
  (let ((full-output (merge-pathnames executable)))
    (main (list "sbcl"
                "--asdf-path"
                (native-namestring
                 (asdf:system-relative-pathname :buildapp "./"))
                "--load-system" "buildapp"
                "--entry" "buildapp:main"
                "--output"
                (native-namestring full-output)))))



(defun buildapp-init ()
  (setf #+sbcl sb-ext:*invoke-debugger-hook*
        #+ccl  ccl::*debugger-hook*
        'command-line-debugger))

#+sbcl (pushnew 'buildapp-init sb-ext:*init-hooks*)

