#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup")

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "alexandria")
  (ql:quickload "cl-readline")
  (ql:quickload "str"))

(require :sb-introspect)

(defpackage :sbcli
  (:use :common-lisp :cffi)
  (:export sbcli *repl-version* *repl-name* *prompt* *prompt2* *ret* *config-file*
           *hist-file* *special* *error*))

(defpackage :sbcli-user
  (:use :common-lisp :sbcli))

(in-package :sbcli)

(defvar *repl-version* "0.1.4")
(defvar *repl-name*    "Veit's REPL for SBCL")
(defvar *prompt*       "sbcl> ")
(defvar *prompt2*       "....> ")
(defvar *ret*          "=> ")
(defvar *config-file*  "~/.sbclirc")
(defvar *hist-file*    "~/.sbcli_history")
(defvar *hist*         (list))
(defvar *pygmentize*   nil)
(defvar *pygmentize-options* (list "-s" "-l" "lisp"))
(defvar *error*        nil)
(declaim (special *special*))

(defun last-nested-expr (s/sexp)
  "From an input with nested parens (or none), return the most nested
function call (or the first thing at the prompt).

(hello (foo (bar:qux *zz* ?
=>
bar:qux
"
  (let* ((input (str:trim s/sexp))
         (last-parens-token (first (last (str:split #\( input)))))
    (first (str:words last-parens-token))))

#+or(nil)
(progn
  (assert (string= "baz:qux"
                   (last-nested-expr "(hello (foo bar (baz:qux zz ?")))
  (assert (string= "baz:qux"
                   (last-nested-expr "(baz:qux zz ?")))
  (assert (string= "qux"
                   (last-nested-expr "(baz (qux ?")))
  (assert (string= "sym"
                   (last-nested-expr "sym ?"))))

(defun read-hist-file ()
  (with-open-file (in *hist-file* :if-does-not-exist :create)
    (loop for line = (read-line in nil nil)
      while line
      ; hack because cl-readline has no function for this. sorry.
      do (cffi:foreign-funcall "add_history"
                               :string line
                               :void))))

(defun update-hist-file (str)
  (with-open-file (out *hist-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (write-line str out)))

(defun end ()
  "Ends the session"
  (write-line "Bye for now.")
  (sb-ext:quit))

(defun reset ()
  "Resets the session environment"
  (delete-package 'sbcli-user)
  (delete-package 'sbcli)
  (defpackage :sbcli (:use :common-lisp))
  (defpackage :sbcli-user
    (:use :common-lisp :sbcli))
  (in-package :sbcli))

(defun split (str chr)
  (loop for i = 0 then (1+ j)
        as j = (position chr str :start i)
        collect (subseq str i j)
        while j))

(defun join (str chr)
  (reduce (lambda (acc x)
            (if (zerop (length acc))
                x
                (concatenate 'string acc chr x)))
          str
          :initial-value ""))

(defun novelty-check (str1 str2)
  (string/= (string-trim " " str1)
            (string-trim " " str2)))

(defun add-res (txt res) (setq *hist* (cons (list txt res) *hist*)))

(defun format-output (&rest args)
  (format (car args) "~a ; => ~a" (caadr args) (cadadr args)))

(defun write-to-file (fname)
  "Writes the current session to a file <filename>"
  (with-open-file (file fname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format file "~{~/sbcli:format-output/~^~%~}" (reverse *hist*))))

(defun help (sym)
  "Gets help on a symbol <sym>"
  (handler-case (inspect (read-from-string sym))
    (error (c) (format *error-output* "Error during inspection: ~a~%" c))))

(defun symbol-documentation (symbol/string)
  "Print the available documentation for this symbol."
  ;; Normally, the documentation function takes as second argument the
  ;; type designator. We loop over each type and print the available
  ;; documentation.
  (handler-case (loop for doc-type in '(variable function structure type setf)
                   with sym = (if (stringp symbol/string)
                                  ;; used from the readline REPL
                                  (read-from-string symbol/string)
                                  ;; used from Slime
                                  symbol/string)
                   for doc = (unless (consp sym)
                               ;; When the user enters :doc 'sym instead of :doc sym
                               (documentation sym doc-type))
                   when doc
                   do (format t "~a: ~a~&" doc-type doc)
                   when (and (equal doc-type 'function)
                             (fboundp sym))
                   do (format t "ARGLIST: ~a~&"
                              (format nil "~(~a~)"
                                      (sb-introspect:function-lambda-list sym))))
    (error (c) (format *error-output* "Error during documentation lookup: ~a~&" c))))

(defun general-help ()
  "Prints a general help message"
  (format t "~a version ~a~%" *repl-name* *repl-version*)
  (write-line "Special commands:")
  (maphash
    (lambda (k v) (format t "  :~a: ~a~%" k (documentation (cdr v) t)))
    *special*)
  (write-line "Currently defined:")
  (print-currently-defined))

(defun print-currently-defined ()
  (do-all-symbols (s *package*)
    (when (and (or (fboundp s) (boundp s)) (eql (symbol-package s) *package*))
      (let ((what (cond ((fboundp s) 'function) ((constantp s) 'constant) (t 'variable))))
        (format t " ~a: ~a (~a) ~a~%" (string-downcase (string s))
                                      (or (documentation s what)
                                          "No documentation")
                                      what
                                      (if (boundp s)
                                        (format nil "(value ~a)" (eval s))
                                        ""))))))

(defun dump-disasm (sym)
  "Dumps the disassembly of a symbol <sym>"
  (handler-case (disassemble (read-from-string sym))
    (unbound-variable (var) (format t "~a~%" var))
    (type-error (err) (format t "~a~%" err))
    (sb-int:compiled-program-error (err) (format t "~a~%" err))
    (undefined-function (fun) (format t "~a~%" fun))))

(defun dump-type (expr)
  "Prints the type of a expression <expr>"
  (handler-case (format t "~a~%" (type-of (eval (read-from-string expr))))
    (unbound-variable (var) (format t "~a~%" var))
    (type-error (err) (format t "~a~%" err))
    (sb-int:compiled-program-error (err) (format t "~a~%" err))
    (undefined-function (fun) (format t "~a~%" fun))))

(defun get-package-for-search (text)
  "Return a list with:
  - the text after the colon or double colon
  - the package name (a string)
  - T if we are looking for an external symbol, NIL for an internal one."
  (let ((pos))
    (cond
      ((setf pos (search "::" text))
       (list (subseq text  (+ pos 2))
             (subseq text 0 pos)
             nil))
      ((setf pos (position #\: text))
       (if (zerop pos)
           (list text nil t)
           (list (subseq text (1+ pos))
                 (subseq text 0 pos)
                 t)))
      (t (list text nil  t)))))

#+or(nil)
(progn
  (assert (equal (list "file-" "uiop" t)
                 (get-package-for-search "uiop:file-")))
  (assert (equal (list "*wil" "uiop" nil)
                 (get-package-for-search "uiop::*wil")))
  (assert (equal (list "*feat" nil t)
                 (get-package-for-search "*feat"))))

(defun list-external-symbols (sym-name pkg-name)
  "List external symbols of PKG-NAME (a string).
  (the symbol name is currently ignored)."
  (declare (ignorable sym-name))
  (assert (stringp pkg-name))
  (loop :for sym :being :the :external-symbols :of (find-package pkg-name)
     :collect (format nil "~(~a:~a~)" pkg-name sym)))

(defun list-internal-symbols (sym-name pkg-name)
  "List internal symbols of the package named PKG-NAME (a string)."
  (declare (ignorable sym-name))
  (assert (stringp pkg-name))
  (loop :for sym :being :the :symbols :of (find-package pkg-name)
     :collect (format nil "~(~a::~a~)" pkg-name sym)))

(defun list-symbols-and-packages (sym-name)
  "Base case, when the user entered a string with no colon that would delimit a package.
  Return the current packages, symbols of the current package, current keywords.
  They are filtered afterwards, in SELECT-COMPLETIONS."
  (declare (ignorable sym-name))
  (concatenate 'list
               (loop :for pkg :in (list-all-packages)
                  :append (loop :for name :in (package-nicknames pkg)
                             :collect (format nil "~(~a:~)" name))
                  :collect (format nil "~(~a:~)" (package-name pkg)))
               (loop :for sym :being :the :symbols :of *package*
                  :collect (string-downcase sym))
               (loop :for kw :being :the :symbols :of (find-package "KEYWORD")
                  :collect (format nil ":~(~a~)" kw))))

(defun select-completions (text items)
  "TEXT is the string entered at the prompt, ITEMS is a list of
strings to match candidates against (for example in the form \"package:sym\")."
  (setf items
        (loop :for item :in items
           :when (str:starts-with-p text item)
           :collect item))
  (unless (cdr items)
    (setf rl:*completion-append-character*
          (if (str:ends-with-p ":" (car items))
              #\nul
              #\space))
    (return-from select-completions items))
  (cons
   (subseq (car items) 0
           (loop :for item :in (cdr items)
              :minimize (or (mismatch (car items) item) (length item))))
   items))

#+or(nil)
(progn
  (assert (member "str:concat"
                  (select-completions "str:con" (list "str:containsp" "str:concat" "str:constant-case"))
                  :test #'string-equal)))

(defun custom-complete (text &optional start end)
  "Custom completer function for readline, triggered when we press TAB.

  START and END are accepted by readline but are not used."
  (declare (ignore start end))
  (when (string-equal text "")
    (return-from custom-complete nil))
  (destructuring-bind (sym-name pkg-name external-p)
      (get-package-for-search (string-upcase text))
    (when (and pkg-name
               (not (find-package pkg-name)))
      (return-from custom-complete nil))
    (select-completions
     (str:downcase text)
     (cond
       ((zerop (length pkg-name))
        (list-symbols-and-packages sym-name))
       (external-p
        (list-external-symbols sym-name pkg-name))
       (t
        (list-internal-symbols sym-name pkg-name))))))

#+or(nil)
(progn
  (assert (member "str:suffixp"
                  (custom-complete "str:suff")
                  :test #'string-equal))
  (assert (member "uiop:file-exists-p"
                  (custom-complete "uiop:file-")
                  :test #'string-equal)))

(defun maybe-highlight (str)
  (if *pygmentize*
    (with-input-from-string (s str)
      (let ((proc (sb-ext:run-program *pygmentize*
                                      *pygmentize-options*
                                      :input s
                                      :output :stream)))
         (read-line (sb-ext:process-output proc) nil "")))
    str))

(defun syntax-hl ()
  (rl:redisplay)
  (let ((res (maybe-highlight rl:*line-buffer*)))
    (format t "~c[2K~c~a~a~c[~aD" #\esc #\return rl:*display-prompt* res #\esc (- rl:+end+ rl:*point*))
    (when (= rl:+end+ rl:*point*)
      (format t "~c[1C" #\esc))
    (finish-output)))

(rl:register-function :complete #'custom-complete)
(rl:register-function :redisplay #'syntax-hl)

;; -1 means take the string as one arg
(defvar *special*
  (alexandria:alist-hash-table
    `(("h" . (1 . ,#'help))
      ("help" . (0 . ,#'general-help))
      ("doc" . (1 . ,#'symbol-documentation))
      ("s" . (1 . ,#'write-to-file))
      ("d" . (1 . ,#'dump-disasm))
      ("t" . (-1 . ,#'dump-type))
      ("q" . (0 . ,#'end))
      ("r" . (0 . ,#'reset))) :test 'equal))

(defun call-special (fundef call args)
  (let ((l (car fundef))
        (fun (cdr fundef))
        (rl (length args)))
    (cond
      ((= -1 l) (funcall fun (join args " ")))
      ((< rl l)
        (format *error-output*
                "Expected ~a arguments to ~a, but got ~a!~%"
                l call rl))
      (t (apply fun (subseq args 0 l))))))

(defun handle-special-input (text)
  (let* ((splt (split text #\Space))
         (k (subseq (car splt) 1 (length (car splt))))
         (v (gethash k *special*)))
    (if v
      (call-special v (car splt) (cdr splt))
      (format *error-output* "Unknown special command: ~a~%" k))))

(defun evaluate-lisp (text parsed)
  "Evaluate (EVAL) the user input.
  In case of evaluation error, print the error.
  Then print the result. Print its multiple values on multiple lines.
  Save the input history.
  Handle the special *, + et all REPL history variables."
  (let ((result-list
         (multiple-value-list
          (handler-case (eval parsed)
            (unbound-variable (var)
              (format *error-output* "~a~%" var))
            (undefined-function (fun)
              (format *error-output* "~a~%" fun))
            (sb-int:compiled-program-error ()
              (format *error-output* "Compiler error.~%"))
            (error (condition)
	      (setf *error* condition)
              (format *error-output* "Evaluation error: ~a~%" condition))))))
    (when result-list
      (add-res text (car result-list))
      (setf +++ ++
            /// //
            *** (car ///)
            ++ +
            // /
            ** (car //)
            + parsed
            / result-list
            * (car result-list))
      (format t "~a~{~s~&~}~%" *ret* result-list))))

(defun handle-lisp (before text)
  (let* ((new-txt (format nil "~a ~a" before text))
         (parsed (handler-case (read-from-string new-txt)
                   (end-of-file () (sbcli new-txt *prompt2*))
                   (error (condition)
                    (format *error-output* "Parser error: ~a~%" condition)))))
    (when parsed (evaluate-lisp text parsed))))

(defun handle-input (before text)
  (if (and (> (length text) 1)
           (string= (subseq text 0 1) ":"))
    (handle-special-input text)
    (handle-lisp before text)))

(defun sbcli (txt p)
  (let ((text
         (handler-case
             (rl:readline :prompt (if (functionp p) (funcall p) p)
                          :add-history t
                          :novelty-check #'novelty-check)
           (sb-sys:interactive-interrupt ()
             (write-char #\linefeed)
             ""))))
    (in-package :sbcli-user)
    (unless text (end))
    (if (string= text "") (sbcli "" *prompt*))
    (when *hist-file* (update-hist-file text))
    (cond
      ((str:ends-with-p " ?" text)
       ;; a trailing " ?" prints the symbol documentation.
       (sbcli::symbol-documentation (last-nested-expr text)))
      (t
       (sbcli::handle-input txt text)))
    (in-package :sbcli)
    (finish-output nil)
    (rl:register-function :redisplay #'syntax-hl)
    (sbcli "" *prompt*)))

(if (probe-file *config-file*)
  (load *config-file*))

(format t "~a version ~a~%" *repl-name* *repl-version*)
(write-line "Press CTRL-D or type :q to exit")
(write-char #\linefeed)
(finish-output nil)

(when *hist-file* (read-hist-file))

(sb-ext:enable-debugger)

(handler-case (sbcli "" *prompt*)
  (sb-sys:interactive-interrupt () (end)))

