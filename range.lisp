(defpackage :range
  (:use common-lisp))

(in-package :range)

(defun build-range (range-string)
  "Given the text within a range expression, e.g.:
   `1..100' or `32,35..47' or `10,9..0', return the corresponding list"
  (declare (string range-string))
  ;; Check that the range is well-formed
  (unless (cl-ppcre:scan "\\d+(,\\d+)?\\.\\.\\d+" range-string)
    (error "Badly formed range [~A]" range-string))
  (let ((sections (cl-ppcre:all-matches-as-strings "\\d+" range-string)))
    sections))
