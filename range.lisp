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
  (let* ((sections (cl-ppcre:all-matches-as-strings "\\d+" range-string))
         (start (read-from-string (first sections)))
         (step (if (= (length sections) 3)
                 (- (read-from-string (second sections))
                    start)
                 1))
         (end (read-from-string (if (= (length sections) 3)
                                  (third sections)
                                  (second sections)))))
    (loop for n from start to end by step collecting n)))

(defun sharp-left-bracket (stream char)
  (declare (ignore char))
  (coerce (loop for char = (read-char stream)
                until (char= char #\])
                collecting char)
          'string))
