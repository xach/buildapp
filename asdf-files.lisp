;;;; asdf-files.lisp

(in-package #:buildapp)

(defun manifest-file-files (file)
  "Return a list of all system files contained in FILE. The syntax is
one namestring per line. Relative namestrings are resolved relative to
the truename of FILE."
  (let ((truename (truename file)))
    (remove-if #'null
               (mapcar (lambda (namestring)
                         (probe-file (merge-pathnames namestring
                                                      truename)))
                       (file-lines file)))))

(defun asdf-path-files (pathname)
  (directory (merge-pathnames "*.asd" pathname)))

(defun asdf-tree-files (pathname)
  (directory (merge-pathnames "**/*.asd" pathname)))

(defun asdf-directive-files (directive-list)
  "Convert a list of directives to a list of pathnames. No two
  pathnames in th eresult have the same pathname-name. A directive
  should be a list of a symbol and a pathname. The directive can be
  one of :MANIFEST-FILE, :ASDF-PATH, or :ASDF-TREE."
  (let ((result '())
        (table (make-hash-table :test 'equalp)))
    (flet ((add-files (files)
             (dolist (file files)
               (unless (gethash (pathname-name file) table)
                 (setf (gethash (pathname-name file) table) file)
                 (push file result)))))
      (loop for (directive pathname) in directive-list
            do (ecase directive
                 (:manifest-file
                  (add-files (manifest-file-files pathname)))
                 (:asdf-path
                  (add-files (asdf-path-files pathname)))
                 (:asdf-tree
                  (add-files (asdf-tree-files pathname)))))
      (nreverse result))))
