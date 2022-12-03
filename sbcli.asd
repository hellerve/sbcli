(in-package asdf-user)

(defsystem sbcli
  :description "REPL for my SBCL needs"
  :long-description "A better REPL for SBCL. 
  sbcli handles errors gracefully, is not too verbose, has readline capabilities, 
  including multiline input and reset, and has optional syntax highlighting 
  capabilities using pygmentize."
  :version "0.1.4"
  :author "Veit Heller <veit@veitheller.de>"
  :license "GPLv3"
  :depends-on
  ("alexandria"
   "cl-readline"
   "str")
  :components
  ((:module ""
    :serial t
    :components
    ((:file "repl"))))
  :build-operation "program-op"
  :entry-point "repl:sbcli"
  :build-pathname #.(merge-pathnames #p"repl" (uiop:getcwd)))
