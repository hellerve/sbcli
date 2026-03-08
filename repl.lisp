#!/usr/bin/env -S sbcl --script
(let ((ql-setup (probe-file (merge-pathnames "quicklisp/setup.lisp"
                                              (user-homedir-pathname)))))
  (when ql-setup (load ql-setup)))

(let ((*standard-output* (make-broadcast-stream)))
  (if (find-package :ql)
      (let ((ql (find-symbol "QUICKLOAD" :ql)))
        (funcall ql "alexandria")
        (funcall ql "cl-readline")
        (funcall ql "str"))
      (progn
        (require :alexandria)
        (require :cl-readline)
        (require :str))))

(require :sb-introspect)

(defpackage :sbcli
  (:use :common-lisp :cffi)
  (:export sbcli *repl-version* *repl-name* *prompt* *prompt2* *ret* *config-file*
           *hist-file* *special* *error* *debug-prompt*))

(defpackage :sbcli-user
  (:use :common-lisp :sbcli))

(in-package :sbcli)

(defvar *repl-version* "0.2.0")
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
(defvar *debug-prompt* (lambda (level) (format nil "debug[~a]> " level)))
(defvar *debug-level*  0)
(defvar *current-frame* nil)
(defvar *breakpoints* nil)
(defvar *breakpoint-counter* 0)
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
  (let ((pkg (find-package :sbcli-user)))
    (do-symbols (s pkg)
      (when (eq (symbol-package s) pkg)
        (makunbound s)
        (fmakunbound s)
        (unintern s pkg))))
  (in-package :sbcli-user))


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
    (error (e) (format t "~a~%" e))))

(defun dump-type (expr)
  "Prints the type of a expression <expr>"
  (handler-case (format t "~a~%" (type-of (eval (read-from-string expr))))
    (error (e) (format t "~a~%" e))))

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

(defun register-redisplay ()
  (if *pygmentize*
      (rl:register-function :redisplay #'syntax-hl)
      (rl:register-function :redisplay #'rl:redisplay)))

(register-redisplay)

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
      ((= -1 l) (funcall fun (str:join " " args)))
      ((< rl l)
        (format *error-output*
                "Expected ~a arguments to ~a, but got ~a!~%"
                l call rl))
      (t (apply fun (subseq args 0 l))))))

(defun handle-special-input (text)
  (let* ((splt (str:split #\Space text))
         (k (subseq (car splt) 1 (length (car splt))))
         (v (gethash k *special*)))
    (if v
      (call-special v (car splt) (cdr splt))
      (format *error-output* "Unknown special command: ~a~%" k))))

(defun find-initial-frame ()
  "Find the EVAL frame as the starting point for debugging."
  (ignore-errors
    (loop for f = (sb-di:top-frame) then (sb-di:frame-down f) ;; toward caller
          while f
          when (eq 'eval (ignore-errors
                           (sb-di:debug-fun-name (sb-di:frame-debug-fun f))))
          return f
          finally (return (sb-di:top-frame)))))

;; Wrappers for sb-debug internals (single point of maintenance)
(defun print-frame-call (frame &optional (stream *standard-output*))
  (sb-debug::print-frame-call frame stream))

(defun debug-eval-in-frame (form frame)
  (let ((sb-debug::*current-frame* frame))
    (sb-debug::debug-eval form)))

(defun code-location-source (location)
  (sb-debug::code-location-source-form location 0))

(defun find-top-frame ()
  "Walk from *current-frame* to the topmost frame."
  (let ((f *current-frame*))
    (loop for up = (ignore-errors (sb-di:frame-up f))
          while up do (setf f up))
    f))

(defun debugger-print-frame (&optional (frame *current-frame*))
  "Print a single frame with its number."
  (when frame
    (format t "~d: " (sb-di:frame-number frame))
    (handler-case (print-frame-call frame)
      (error () (format t "<error printing frame>")))
    (terpri)))

(defun debugger-backtrace (&optional (count 20))
  "Show backtrace starting from the most recent frame."
  (call-with-pager
    (lambda ()
      (loop for f = (find-top-frame) then (ignore-errors (sb-di:frame-down f))
            for i from 0 below count
            while f
            do (format t "~:[  ~;> ~]~d: "
                       (eq f *current-frame*)
                       (sb-di:frame-number f))
               (handler-case (print-frame-call f)
                 (error () (format t "<error printing frame>")))
               (terpri)))))

(defun debugger-frame-move (dir-fn fail-msg)
  "Move the current frame in the direction given by DIR-FN."
  (let ((next (ignore-errors (funcall dir-fn *current-frame*))))
    (if next
        (progn (setf *current-frame* next) (debugger-print-frame))
        (format *error-output* fail-msg))))

(defun debugger-go-to-frame (n)
  "Navigate to frame number N."
  (loop for f = (find-top-frame) then (ignore-errors (sb-di:frame-down f))
        while f
        when (= (sb-di:frame-number f) n)
        do (setf *current-frame* f)
           (debugger-print-frame)
           (return)
        finally (format *error-output* "Frame ~d not found.~%" n)))

(defun debugger-locals ()
  "Show local variables in the current frame."
  (handler-case
      (let* ((debug-fun (sb-di:frame-debug-fun *current-frame*))
             (vars (sb-di::debug-fun-debug-vars debug-fun)))
        (if (and vars (> (length vars) 0))
            (loop for var across vars
                  do (format t "  ~a = ~a~%"
                             (sb-di:debug-var-symbol var)
                             (handler-case
                                 (format nil "~s"
                                         (sb-di:debug-var-value var *current-frame*))
                               (error () "<unavailable>"))))
            (format t "No local variables.~%")))
    (error (c)
      (format *error-output* "Cannot inspect locals: ~a~%" c))))

(defun debugger-source ()
  "Show source for the current frame."
  (handler-case
      (let ((loc (sb-di:frame-code-location *current-frame*)))
        (if loc
            (handler-case
                (format t "~s~%" (code-location-source loc))
              (error () (format t "Source not available.~%")))
            (format t "No code location.~%")))
    (error (c)
      (format *error-output* "Cannot show source: ~a~%" c))))

(defun call-with-pager (fn)
  "Call FN with output captured, then display through a pager."
  (let ((output (with-output-to-string (*standard-output*)
                  (funcall fn))))
    (handler-case
        (let* ((pager-cmd (or (sb-ext:posix-getenv "PAGER") "less"))
               (pager-args (if (search "less" pager-cmd) '("-FRX") nil)))
          (with-input-from-string (in output)
            (sb-ext:run-program "/usr/bin/env" (cons pager-cmd pager-args)
                                :input in
                                :output t
                                :wait t)))
      (error ()
        (write-string output *standard-output*)))))

(defun map-code-locations (debug-fun fn)
  "Call FN with index and location for each code location in DEBUG-FUN."
  (let ((i 0))
    (sb-di:do-debug-fun-blocks (block debug-fun)
      (sb-di:do-debug-block-locations (loc block)
        (funcall fn i loc)
        (incf i)))
    i))

(defun nth-code-location (debug-fun n)
  "Get the Nth code location from DEBUG-FUN."
  (map-code-locations debug-fun
    (lambda (i loc)
      (when (= i n) (return-from nth-code-location loc))))
  nil)

(defun list-code-locations ()
  "List possible breakpoint locations in the current frame's function."
  (handler-case
      (let* ((debug-fun (sb-di:frame-debug-fun *current-frame*))
             (count (map-code-locations debug-fun
                      (lambda (i loc) (format t "  ~d: ~a~%" i loc)))))
        (when (zerop count)
          (format t "No code locations available.~%")))
    (error (c)
      (format *error-output* "Cannot list locations: ~a~%" c))))

(defun debugger-set-breakpoint (n)
  "Set a breakpoint at code location N in the current frame's function."
  (handler-case
      (let* ((debug-fun (sb-di:frame-debug-fun *current-frame*))
             (loc (nth-code-location debug-fun n)))
        (if loc
            (let* ((bp (sb-di:make-breakpoint #'breakpoint-hook loc
                                              :kind :code-location))
                   (id (incf *breakpoint-counter*)))
              (sb-di:activate-breakpoint bp)
              (push (list id bp loc) *breakpoints*)
              (format t "Breakpoint ~a set.~%" id))
            (format *error-output* "No code location ~a.~%" n)))
    (error (c)
      (format *error-output* "Cannot set breakpoint: ~a~%" c))))

(defun debugger-list-breakpoints ()
  "List all active breakpoints."
  (if *breakpoints*
      (loop for (id bp loc) in *breakpoints*
            do (format t "  ~a: ~a (~:[inactive~;active~])~%"
                       id loc (sb-di:breakpoint-active-p bp)))
      (format t "No breakpoints set.~%")))

(defun debugger-delete-breakpoint (n)
  "Delete breakpoint N."
  (let ((entry (find n *breakpoints* :key #'first)))
    (if entry
        (progn (sb-di:deactivate-breakpoint (second entry))
               (setf *breakpoints* (remove entry *breakpoints*))
               (format t "Breakpoint ~a deleted.~%" n))
        (format *error-output* "No breakpoint ~a.~%" n))))

(defun breakpoint-hook (frame bp)
  "Called when a breakpoint is hit."
  (let* ((*current-frame* frame)
         (entry (find bp *breakpoints* :key #'second)))
    (when entry
      (format *error-output* "~&Hit breakpoint ~a~%" (first entry)))
    (restart-case
        (sbcli-debugger (make-condition 'simple-condition
                                        :format-control "Breakpoint hit"
                                        :format-arguments nil))
      (continue ()
        :report "Continue from breakpoint"
        nil))))

(defun try-invoke-restart (name condition &optional msg)
  "Try to invoke restart NAME. Print MSG if not found."
  (let ((r (find-restart name condition)))
    (if r (invoke-restart r)
        (format *error-output* "~a~%"
                (or msg (format nil "No ~(~a~) restart available." name))))))

(defun debugger-help ()
  "Show debugger help."
  (format t "Debugger commands:
  bt [N]         Backtrace (default 20 frames)
  up, u          Move up the call stack (toward caller)
  down, d        Move down the call stack (toward callee)
  frame N, f N   Go to frame N
  locals, l      Show local variables
  source, src    Show source for current frame
  print, p       Print current frame
  break N, br N  Set breakpoint at code location N
  list-breaks, lb   List breakpoints
  delete-break N, db N  Delete breakpoint N
  list-locations, ll  List breakpoint locations
  step/next/out  Stepping (requires (step ...) form)
  abort, a       Abort to toplevel
  help, h, ?     Show this help
  <number>       Invoke restart by number
  <expression>   Evaluate Lisp expression (frame-aware)
  Ctrl-D         Abort to toplevel~%"))

(defun sbcli-debugger (condition)
  "Interactive debugger for sbcli."
  (when (typep condition 'error)
    (setf *error* condition))
  (let* ((*debug-level* (1+ *debug-level*))
         (*current-frame* (find-initial-frame))
         (restarts (compute-restarts condition)))
    (format *error-output* "~&~a~%~%" condition)
    (format *error-output* "Available restarts:~%")
    (loop for restart in restarts for i from 0
          do (format *error-output* "  ~a: [~a] ~a~%"
                     i (restart-name restart) restart))
    (terpri *error-output*)
    (when *current-frame* (debugger-print-frame))
    (format *error-output* "~%Type 'help' for debugger commands.~%")
    (force-output *error-output*)
    (loop
      (let ((input
              (handler-case
                  (rl:readline :prompt (funcall *debug-prompt* *debug-level*))
                (sb-sys:interactive-interrupt ()
                  (terpri) (try-invoke-restart 'abort condition) nil))))
        (unless input
          (terpri) (try-invoke-restart 'abort condition) (return))
        (let ((trimmed (str:trim input)))
          (if (zerop (length trimmed)) nil
              ;; Pure number → restart selection
              (if (every #'digit-char-p trimmed)
                  (let ((n (parse-integer trimmed)))
                    (if (< n (length restarts))
                        (invoke-restart-interactively (nth n restarts))
                        (format *error-output* "No restart ~a (0-~a available).~%"
                                n (1- (length restarts)))))
                  ;; Otherwise: command + args
                  (let* ((space-pos (position #\Space trimmed))
                         (cmd (string-downcase
                                (if space-pos (subseq trimmed 0 space-pos) trimmed)))
                         (args (if space-pos
                                   (str:trim (subseq trimmed (1+ space-pos))) ""))
                         (int-arg (and (> (length args) 0)
                                       (ignore-errors (parse-integer args)))))
                    (cond
                      ((member cmd '("bt" "backtrace") :test #'string=)
                       (debugger-backtrace (or int-arg 20)))
                      ((member cmd '("up" "u") :test #'string=)
                       (debugger-frame-move #'sb-di:frame-down ;; toward caller
                                            "Already at the top.~%"))
                      ((member cmd '("down" "d") :test #'string=)
                       (debugger-frame-move #'sb-di:frame-up ;; toward callee
                                            "Already at the bottom.~%"))
                      ((member cmd '("frame" "f") :test #'string=)
                       (if int-arg (debugger-go-to-frame int-arg)
                           (format *error-output* "Usage: frame <number>~%")))
                      ((member cmd '("locals" "l") :test #'string=) (debugger-locals))
                      ((member cmd '("source" "src") :test #'string=) (debugger-source))
                      ((member cmd '("print" "p") :test #'string=) (debugger-print-frame))
                      ((member cmd '("break" "br") :test #'string=)
                       (if int-arg (debugger-set-breakpoint int-arg)
                           (format *error-output* "Usage: break <location>~%")))
                      ((member cmd '("list-breaks" "lb") :test #'string=)
                       (debugger-list-breakpoints))
                      ((member cmd '("delete-break" "db") :test #'string=)
                       (if int-arg (debugger-delete-breakpoint int-arg)
                           (format *error-output* "Usage: delete-break <id>~%")))
                      ((member cmd '("list-locations" "ll") :test #'string=)
                       (list-code-locations))
                      ((string= cmd "step")
                       (try-invoke-restart 'sb-ext:step-into condition
                         "No step-into restart. Use (step ...) to enable stepping."))
                      ((string= cmd "next")
                       (try-invoke-restart 'sb-ext:step-next condition))
                      ((string= cmd "out")
                       (try-invoke-restart 'sb-ext:step-out condition))
                      ((member cmd '("abort" "a") :test #'string=)
                       (try-invoke-restart 'abort condition))
                      ((member cmd '("help" "h" "?") :test #'string=) (debugger-help))
                      (t (handler-case
                             (let ((results (multiple-value-list
                                              (debug-eval-in-frame
                                                (read-from-string trimmed)
                                                *current-frame*))))
                               (format t "~{~s~&~}" results))
                           (error (c) (format *error-output* "~a~%" c)))))))))
      (finish-output)))))

(defun evaluate-lisp (text parsed)
  "Evaluate (EVAL) the user input.
  In case of error, enter the interactive debugger with available restarts.
  Then print the result. Print its multiple values on multiple lines.
  Save the input history.
  Handle the special *, + et all REPL history variables."
  (let ((result-list
         (multiple-value-list
          (restart-case
              (handler-bind
                  ((error #'sbcli-debugger))
                (eval parsed))
            (abort ()
              :report "Return to sbcli toplevel"
              (values))))))
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
    (unless text (end))
    (if (string= text "") (sbcli "" *prompt*))
    (when *hist-file* (update-hist-file text))
    (cond
      ((str:ends-with-p " ?" text)
       ;; a trailing " ?" prints the symbol documentation.
       (sbcli::symbol-documentation (last-nested-expr text)))
      (t
       (sbcli::handle-input txt text)))
    (finish-output nil)
    (register-redisplay)
    (sbcli "" *prompt*)))

(if (probe-file *config-file*)
  (load *config-file*))

(format t "~a version ~a~%" *repl-name* *repl-version*)
(write-line "Press CTRL-D or type :q to exit")
(write-char #\linefeed)
(finish-output nil)

(when *hist-file* (read-hist-file))

(sb-ext:enable-debugger)
(setf sb-ext:*stepper-hook* #'sbcli-debugger)

(let ((*package* (find-package :sbcli-user)))
  (handler-case (sbcli "" *prompt*)
    (sb-sys:interactive-interrupt () (end))))

