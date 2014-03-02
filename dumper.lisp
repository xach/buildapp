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

;;;; dumper.lisp

(in-package #:buildapp)

(defclass dumper ()
  ((package
    :initarg :package
    :accessor package
    :initform (gensym "DUMPER"))
   (actions
    :initarg :actions
    :accessor actions
    :initform nil)
   (entry
    :initarg :entry
    :accessor entry
    :initform nil)
   (dispatched-entries
    :initarg :dispatched-entries
    :accessor dispatched-entries
    :initform nil)
   (asdf-directives
    :initarg :asdf-directives
    :accessor asdf-directives
    :initform nil)
   (load-paths
    :initarg :load-paths
    :accessor load-paths
    :initform nil)
   (sbcl
    :initarg :sbcl
    :accessor sbcl
    :initform "sbcl")
   (ccl
    :initarg :ccl
    :accessor ccl
    :initform "ccl")
   (output
    :initarg :output
    :accessor output
    :initform nil)
   (logfile
    :initarg :logfile
    :accessor logfile
    :initform nil)
   (dumpfile-copy
    :initarg :dumpfile-copy
    :accessor dumpfile-copy
    :initform nil)
   (core-only
    :initarg :core-only
    :accessor core-only
    :initform nil)
   (compress-core
    :initarg :compress-core
    :accessor compress-core
    :initform nil)
   (dynamic-space-size
    :initarg :dynamic-space-size
    :accessor dynamic-space-size
    :initform nil)))

(defgeneric needs-asdf-p (dumper)
  (:method (dumper)
    (or
     (find :load-system (actions dumper) :key 'first)
     (asdf-directives dumper))))

(defmethod print-object ((dumper dumper) stream)
  (print-unreadable-object (dumper stream :type t)
    (format stream "~A~@[ ~A~]"
            (output dumper)
            (entry dumper))))

(defgeneric asdf-system-files (dumper)
  (:method (dumper)
    (asdf-directive-files (asdf-directives dumper))))

(defun dispatched-entry-form (dispatched-entries)
  (let ((default nil))
    (flet ((one-clause (entry)
             (let* ((binary-name (binary-name entry))
                    (entry-function (entry entry))
                    (call `(,entry-function ,(get-args))))
               (if (default-entry-p entry)
                   (progn (setf default entry) nil)
                   (list
                    `((string= binary-name ,binary-name)
                      (return ,call)))))))
      `(with-simple-restart (abort "Exit application")
         (lambda ()
           (block nil
             (let ((binary-name (pathname-name (pathname (first ,(get-args))))))
               (cond ,@(mapcan #'one-clause dispatched-entries))
               ,@(if default
                     (list
                      `(,(entry default) ,(get-args)))
                     (list
                      `(format *error-output* "Unknown dispatch name '~A', quitting~%"
                               binary-name)
                      (macroexpand-1 (quit 1)))))))))))

(defgeneric entry-function-form (dumper)
  (:method (dumper)
    (cond ((entry dumper)
           `(lambda ()
              (with-simple-restart (abort "Exit application")
                (,(entry dumper) ,(get-args)))))
          ((dispatched-entries dumper)
           (dispatched-entry-form (dispatched-entries dumper))))))

(defgeneric entry-function-check-form (dumper)
  (:method (dumper)
    (cond ((entry dumper)
           (pseudosymbol-check-form (entry dumper)))
          ((dispatched-entries dumper)
           `(progn
              ,@(mapcar (lambda (dentry)
                          (pseudosymbol-check-form (entry dentry)))
                        (dispatched-entries dumper)))))))


;;; Dumpable forms are both evaluated and saved away for later use in
;;; the dumper file.

(defparameter *dumpable-forms* (make-hash-table))

(defmacro dumpable (name &body body)
  (assert (not (cdr body)))
  `(progn
     (setf (gethash ',name *dumpable-forms*) ',(first body))
     ,@body))

(defun dump-form (name)
  (gethash name *dumpable-forms*))

