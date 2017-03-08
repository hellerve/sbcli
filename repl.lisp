#!/usr/local/bin/sbcl --script
(load "~/quicklisp/setup")

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "cl-readline"))

(defvar *repl-version* "0.0.1")
(defvar *repl-name*    "Veit's REPL for SBCL")
(defvar *prompt*       "sbcl> ")
(defvar *ret*          "=> ")
(defvar *config-file*  "~/.sbclirc")

(format t "~a version ~a~%" *repl-name* *repl-version*)
(format t "Press CTRL-C or CTRL-D or type :q to exit~%~%")
(finish-output nil)

(defun end ()
  (format t "Bye for now.~%")
  (exit))

(defun main ()
  (let ((text
          (rl:readline :prompt *prompt*
                       :add-history t)))
    (if (not text) (end))
    (let ((parsed
           (handler-case (read-from-string text)
             (error (condition) (format t "Parser error: ~a~%" condition)))))
      (if parsed
        (let ((res
                (handler-case (eval parsed)
                  (unbound-variable (var) (format t "~a~%" var))
                  (undefined-function (fun) (format t "~a~%" fun))
                  (sb-int:compiled-program-error () (format t "Compiler error.~%"))
                  (error (condition) (format t "Compiler error: ~a~%" condition)))))
          (if (eq res :q) (end))
          (if res (format t "~a~a~%" *ret* res)))))
      (finish-output nil)
      (main)))

(if (probe-file *config-file*)
  (load *config-file*))

(handler-case (main)
  (sb-sys:interactive-interrupt () (end)))
