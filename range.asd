(asdf:defsystem :range
  :description "Haskell style ranges in Common Lisp"
  :depends-on (#:cl-ppcre #:named-readtables)
  :components ((:file "range")))
