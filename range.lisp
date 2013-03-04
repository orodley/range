(defpackage :range
  (:use common-lisp named-readtables))
(in-package :range)

(defun build-range (range-string)
  "Given the text within a range expression, e.g.:
   `1..100' or `32,35..47' or `10,9..0', return the corresponding list"
  (declare (string range-string))
  (unless (cl-ppcre:scan "^ *\\d+( *, *\\d+)? *\\.\\. *\\d+$" range-string)
    (error "Badly formed range #[~A]" range-string))
  (let* ((sections (cl-ppcre:all-matches-as-strings "\\d+" range-string))
         (start (read-from-string (first sections)))
         (step (if (= (length sections) 3)
                 (- (read-from-string (second sections))
                    start)
                 1))
         (end (read-from-string (if (= (length sections) 3)
                                  (third sections)
                                  (second sections)))))
    (list 'quote (cond
                   ((zerop step) 
                    (error "Step is zero in #[~A]" range-string))
                   ((plusp step)
                    (loop for n from start to end by step collecting n))
                   ((minusp step)
                    (loop for n from start downto end by (abs step)
                          collecting n))))))

(defun sharp-left-bracket (stream char args)
  (declare (ignore char args))
  (build-range (coerce (loop for char = (read-char stream)
                             until (char= char #\])
                             collecting char)
                       'string)))

(defreadtable range
  (:merge :standard)
  (:dispatch-macro-char #\# #\[ #'sharp-left-bracket))
