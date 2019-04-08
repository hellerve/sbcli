#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup")

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "alexandria")
  (ql:quickload "cl-readline"))

(defpackage :sbcli
  (:use :common-lisp))

(in-package :sbcli)

(defvar *repl-version* "0.0.5")
(defvar *repl-name*    "Veit's REPL for SBCL")
(defvar *prompt*       "sbcl> ")
(defvar *prompt2*       "....> ")
(defvar *ret*          "=> ")
(defvar *config-file*  "~/.sbclirc")
(defvar *ans*          nil)
(defvar *hist*         (list))

(defun end ()
  (format t "Bye for now.~%")
  (sb-ext:quit))

(defun reset ()
  (delete-package 'sbcli)
  (defpackage :sbcli (:use :common-lisp))
  (in-package :sbcli))

(defun split (str chr)
  (loop for i = 0 then (1+ j)
        as j = (position chr str :start i)
        collect (subseq str i j)
        while j))

(defun novelty-check (str1 str2)
  (string/= (string-trim " " str1)
            (string-trim " " str2)))

(defun add-res (txt res) (setq *hist* (cons (list txt res) *hist*)))

(defun format-output (&rest args)
  (format (car args) "~a ; => ~a" (caadr args) (cadadr args)))

(defun write-to-file (fname)
  (with-open-file (file fname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format file "~{~/sbcli:format-output/~^~%~}" (reverse *hist*))))

(defun custom-complete (text start end)
  (declare (ignore start) (ignore end))
  (labels ((common-prefix (items)
             (let ((lst 0))
              (loop for n from 1 below (reduce #'min (mapcar #'length items)) do
                (when (every (lambda (x)
                             (char= (char (car items) n)
                                    (char x           n)))
                         (cdr items))
                  (setf lst n)))
              (write lst)
              (subseq (car items) 0 (+ lst 1))))
           (starts-with (text)
             (lambda (sym)
               (let* ((symstr (string-downcase sym))
                      (cmp (subseq symstr 0 (min (length symstr) (length text)))))
                 (string= text cmp))))
           (select-completions (list)
             (let* ((els (remove-if-not (starts-with text)
                                       (mapcar #'string list)))
                    (els (if (cdr els) (cons (common-prefix els) els) els)))
                (if (string= text (string-downcase text))
                  (mapcar #'string-downcase els)
                  els)))
           (get-all-symbols ()
             (let ((lst ()))
               (do-all-symbols (s lst)
                 (when (fboundp s) (push s lst)))
               lst)))
      (select-completions (get-all-symbols))))

(rl:register-function :complete #'custom-complete)

(defun main (txt p)
  (let ((text
          (rl:readline :prompt (if (functionp p) (funcall p) p)
                       :add-history t
                       :novelty-check #'novelty-check)))
    (if (not text) (end))
    (if (string= text "") (main "" *prompt*))
    (cond
      ((and (> (length text) 1) (string= (subseq text 0 2) ":h"))
        (let ((splt (split text #\Space)))
          (if (/= (length splt) 2)
            (format t "Type :h <symbol> to get help on a symbol.~%")
            (handler-case (inspect (intern (cadr splt)))
              (error (condition)
                (format t "Error during inspection: ~a~%" condition))))))
      ((and (> (length text) 1) (string= (subseq text 0 2) ":s"))
        (let ((splt (split text #\Space)))
          (if (/= (length splt) 2)
            (format t "Type :s <file> to save the current session to a file.~%")
            (write-to-file (cadr splt)))))
      (t
        (let* ((new-txt (format nil "~a ~a" txt text))
               (parsed (handler-case (read-from-string new-txt)
                         (end-of-file () (main new-txt *prompt2*))
                         (error (condition)
                          (format t "Parser error: ~a~%" condition)))))
          (if parsed
            (progn
              (setf *ans*
                      (handler-case (eval parsed)
                        (unbound-variable (var) (format t "~a~%" var))
                        (undefined-function (fun) (format t "~a~%" fun))
                        (sb-int:compiled-program-error ()
                          (format t "Compiler error.~%"))
                        (error (condition)
                          (format t "Compiler error: ~a~%" condition))))
              (if (eq *ans* :q) (end))
              (if (eq *ans* :r) (reset))
              (add-res text *ans*)
              (if *ans* (format t "~a~a~%" *ret* *ans*)))))))
    (finish-output nil)
    (main "" *prompt*)))

(if (probe-file *config-file*)
  (load *config-file*))

(format t "~a version ~a~%" *repl-name* *repl-version*)
(format t "Press CTRL-C or CTRL-D or type :q to exit~%~%")
(finish-output nil)

(handler-case (main "" *prompt*)
  (sb-sys:interactive-interrupt () (end)))
