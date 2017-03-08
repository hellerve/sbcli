#!/usr/local/bin/sbcl --noinform --script
(load "~/quicklisp/setup")

(ql:quickload "cl-readline")

(defun main ()
  (let* ((text
          (rl:readline :prompt (format nil "sbcl> ")
                       :add-history t))
         (parsed
          (handler-case (read-from-string text)
            (sb-int:simple-reader-error () (format t "erroneous parse.~%")))))
    (if (not (eq parsed nil))
      (let ((res
              (handler-case (eval parsed)
                (unbound-variable () (format t "unbound variable.~%"))
                (sb-int:compiled-program-error () (format t "compiler error.~%")))))
        (when (eq res :q)
          (format t "Bye for now.~%")
          (exit))
        (when (not (eq res nil))
          (format t "=> ~a~%" res)
          (finish-output nil))))
    (main)))

(main)
