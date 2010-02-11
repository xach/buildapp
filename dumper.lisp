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
   (asdf-paths
    :initarg :asdf-paths
    :accessor asdf-paths
    :initform nil)
   (asdf-trees
    :initarg :asdf-trees
    :accessor asdf-trees
    :initform nil)
   (load-paths
    :initarg :load-paths
    :accessor load-paths
    :initform nil)
   (sbcl
    :initarg :sbcl
    :accessor sbcl
    :initform "sbcl")
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
    :initform nil)))

(defgeneric needs-asdf-p (dumper)
  (:method (dumper)
    (find :load-system (actions dumper) :key 'first)))

(defmethod print-object ((dumper dumper) stream)
  (print-unreadable-object (dumper stream :type t)
    (format stream "~A~@[ ~A~]"
            (output dumper)
            (entry dumper))))

(defgeneric asdf-system-directories (dumper)
  (:method (dumper)
    (let ((result (mapcar #'directorize (asdf-paths dumper))))
      (dolist (root (asdf-trees dumper) (nreverse result))
        (dolist (directory (all-asdf-directories root))
          (pushnew directory result :test #'string=))))))

(defparameter *dumpable-forms* (make-hash-table))

(defmacro dumpable (name &body body)
  (assert (not (cdr body)))
  `(progn
     (setf (gethash ',name *dumpable-forms*) ',(first body))
     ,@body))

(defun dump-form (name)
  (gethash name *dumpable-forms*))
