(asdf:defsystem :range
  :description "Haskell style ranges in Common Lisp"
  :depends-on (#:cl-ppcre)
  :components ((:file "range")))
