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

;;;; utils.lisp

(in-package #:buildapp)

;;; interoperability
(defun get-args ()
  #+sbcl 'sb-ext:*posix-argv*
  #+ccl  '(ccl::command-line-arguments))

(defmacro backtrace-as-list ()
  #+sbcl '(sb-debug:backtrace-as-list)
  #+ccl  '(ccl::backtrace-as-list))

(defmacro quit (&optional (errno 0))
  #+sbcl `(sb-ext:exit :code ,errno)
  #+ccl  `(ccl:quit ,errno))

(defmacro run-program (program args)
  (let ((func   #+sbcl 'sb-ext:run-program
                #+ccl  'ccl:run-program)
        (search #+sbcl '(:search t)))
    `(,func ,program ,args
            :input nil
            :output *standard-output*
            ,@search)))

(defun native-namestring (namestring)
  (let ((p (pathname namestring)))
    #+sbcl (sb-ext:native-namestring p)
    #+ccl  (ccl:native-translated-namestring p)))

(defparameter *alphabet*
  (concatenate 'string
               "abcdefghijklmnopqrstuvwxyz"
               "0123456789"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun random-string (length)
  "Return a random string with LENGTH characters."
  (let ((string (make-string length)))
    (map-into string (lambda (char)
                       (declare (ignore char))
                       (aref *alphabet* (random (length *alphabet*))))
              string)))

(defun call-with-temporary-open-file (template fun &rest open-args
                                      &key element-type external-format)
  "Call FUN with two arguments: an open output stream and a file
name. When it returns, the file is deleted. TEMPLATE should be a
pathname that can be used as a basis for the temporary file's
location."
  (declare (ignorable element-type external-format))
  (flet ((new-name ()
           (make-pathname :name (concatenate 'string
                                             (pathname-name template)
                                             "-"
                                             (random-string 8))
                          :defaults template)))
    (let (try stream)
      (tagbody
       :retry
         (setf try (new-name))
         (unwind-protect
              (progn
                (setf stream (apply #'open try
                                    :if-exists nil
                                    :direction :output
                                    open-args))
                (unless stream
                  (go :retry))
                (funcall fun stream try))
           (when stream
             (close stream)
             (ignore-errors (delete-file try))))))))

(defmacro with-tempfile ((stream (template file) &rest open-args) &body body)
  `(call-with-temporary-open-file ,template
                                  (lambda (,stream ,file)
                                    ,@body)
                                  ,@open-args))

(defclass pseudosymbol ()
  ((package-string
    :initarg :package-string
    :accessor package-string)
   (symbol-string
    :initarg :symbol-string
    :accessor symbol-string)))

(defmethod print-object ((pseudosymbol pseudosymbol) stream)
  (format stream "~A::~A"
          (package-string pseudosymbol)
          (symbol-string pseudosymbol)))

(defun make-pseudosymbol (string)
  (let* ((package-start 0)
         (package-end (position #\: string))
         (symbol-start (and package-end (position #\: string
                                                  :start package-end
                                                  :test-not #'eql)))
         (package (if package-end
                      (subseq string package-start package-end)
                      "cl-user"))
         (symbol (if symbol-start
                     (subseq string symbol-start)
                     string)))
    (make-instance 'pseudosymbol
                   :package-string package
                   :symbol-string symbol)))

(defclass dispatched-entry ()
  ((binary-name
    :initarg :binary-name
    :accessor binary-name
    :initform nil)
   (entry
    :initarg :entry
    :accessor entry
    :initform ""))
  (:documentation "A dispatched entry is used to select an entry point
  depending on the name of the binary that invoked the application. If
  the binary name is empty, it is considered the default entry if no
  match is found."))

(defmethod print-object ((dispatched-entry dispatched-entry) stream)
  (print-unreadable-object (dispatched-entry stream :type t)
    (format stream "~A/~A"
            (binary-name dispatched-entry)
            (entry dispatched-entry))))

(define-condition malformed-dispatch-entry (error) ())

(defun make-dispatched-entry (string)
  (let ((slash (position #\/ string)))
    (unless slash
      (error 'malformed-dispatch-entry))
    (let ((binary-name (subseq string 0 slash))
          (entry (make-pseudosymbol (subseq string (1+ slash)))))
      (make-instance 'dispatched-entry
                     :binary-name binary-name
                     :entry entry))))

(defun default-entry-p (dispatch-entry)
  (zerop (length (binary-name dispatch-entry))))

(defun directorize (namestring)
  (concatenate 'string (string-right-trim "/" namestring) "/"))

(defun all-asdf-directories (root)
  "Return a list of all ASDF files in the directory tree at ROOT."
  (remove-duplicates
   (mapcar #'directory-namestring
           (directory (merge-pathnames "**/*.asd"
                                       (pathname (directorize root)))))
   :test #'string=))

(defun copy-file (input output &key (if-exists :supersede))
  (with-open-file (input-stream input)
    (with-open-file (output-stream output :direction :output
                                   :if-exists if-exists)
      (loop for char = (read-char input-stream nil)
            while char do (write-char char output-stream)))))

(defun file-lines (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line collect line)))

;; Cribbed from alexandria
(defun flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))
