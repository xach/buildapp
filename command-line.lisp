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

;;;; command-line.lisp

(in-package #:buildapp)

(define-condition command-line-error (error) ())

(define-condition odd-number-of-arguments (command-line-error) ()
  (:report "Odd number of arguments -- all arguments are in \"--flag value\" pairs"))

(define-condition provided-argument-error (command-line-error)
  ((flag
    :initarg :flag
    :accessor argument-error-flag)
   (description
    :initarg :description
    :accessor argument-error-description)
   (extra-info
    :initarg :extra-info
    :accessor argument-error-extra-info))
  (:default-initargs
   :flag nil
   :description "Argument error on"
   :extra-info nil)
  (:report (lambda (condition stream)
             (format stream "~A ~A~@[~A~]"
                     (argument-error-description condition)
                     (argument-error-flag condition)
                     (argument-error-extra-info condition)))))

(define-condition required-argument-missing (provided-argument-error) ()
  (:default-initargs
   :description "Required argument"
   :extra-info " not provided"))

(define-condition unknown-argument (provided-argument-error) ()
  (:default-initargs
   :description "Unknown argument"))

(define-condition missing-output-argument (required-argument-missing) ()
  (:default-initargs :flag "--output"))

(define-condition duplicate-argument (provided-argument-error) ()
  (:default-initargs
   :description "Duplicate argument"
   :extra-info " -- must be provided at most once"))

(defun argument-keyword (argument)
  "Convert a command-line argument to a keyword symbol."
  (find-symbol (string-upcase (subseq argument 2)) :keyword))

(defun command-line-dumper (args)
  (when (oddp (length args))
    (error 'odd-number-of-arguments))
  (let ((plan (make-instance 'dumper)))
    (loop
      (when (endp args)
        (unless (output plan)
          (error 'missing-output-argument))
        (return plan))
      (let* ((argument (pop args))
             (value (pop args))
             (keyword (argument-keyword argument)))
        (unless value
          (error 'missing-argument))
        (case keyword
          ((:load :load-system :require :eval)
           (push (list keyword value) (actions plan)))
          (:asdf-path
           (push value (asdf-paths plan)))
          (:asdf-tree
           (push value (asdf-trees plan)))
          (:load-path
           (push value (load-paths plan)))
          (:output
           (when (output plan)
             (error 'duplicate-argument :flag argument))
           (setf (output plan) value))
          (:logfile
           (when (logfile plan)
             (error 'duplicate-argument :flag argument))
           (setf (logfile plan) value))
          (:dumpfile-copy
           (setf (dumpfile-copy plan) value))
          (:sbcl
           (when (sbcl plan)
             (setf (sbcl plan) value)))
          (:entry
           (when (entry plan)
             (error 'duplicate-argument :flag argument))
           (setf (entry plan) value))
          (t
           (error 'unknown-argument :flag argument)))))))




