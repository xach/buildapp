(in-package :cl-user)

(defun main (argv)
  (declare (ignore argv))
  (format t "~&This is the test application~%")
  (uiop:quit 0))
