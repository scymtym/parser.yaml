;;;; package.lisp --- Unit tests for the grammar of the parser.yaml system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Note: The basic idea, structure and accompanying data files (in
;;;; the data directory) of these unit tests are based on the version
;;;; 0.9.3 of the YamlReference Haskell package.

(cl:in-package #:parser.yaml.test)

(in-suite parser.yaml)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %print-without-earmuffs (stream thing colon? at?)
    (declare (ignore colon? at?))
    (write-string (ecase thing
                    (*c*              "c")
                    (*n*              "n")
                    (*chomping-style* "t"))
                  stream)))

(defun %should-fail? (spec)
  (loop with position = 0
        while (when-let ((position1 (search (coerce '(#\Newline #\!) 'string) spec :start2 position)))
                (if (not (eql position1 (search (coerce '(#\Newline #\! #\C #\o) 'string) spec :start2 position)))
                    (return t)
                    (setf position (1+ position1))))))

;; This finds all test case files, extract test case parameters
;; (encoded in the file names) and generates one test case for each
;; file.
(macrolet
    ((frob ()
       (let+ (((&flet find-data-files (type)
                 (directory
                  (merge-pathnames
                   (make-pathname :name :wild :type type)
                   (asdf:system-relative-pathname :parser.yaml "test/data/dummy")))))
              (inputs (find-data-files "input"))
              ((&flet test-case (input)
                 (let+ ((spec (pathname-name input))
                        ((rule bindings tag) (parse 'test-case-spec spec))
                        (name (apply #'symbolicate
                                     rule
                                     (format nil "~@[~{~{.~A=~A~}~}~]" bindings)
                                     (when tag (list #\. (string-upcase tag)))))
                        (output (merge-pathnames
                                 (format nil "~(~A~@[~{~{.~/parser.yaml.test::%print-without-earmuffs/=~A~}~}~]~)~@[.~A~].output"
                                         rule bindings tag)
                                 input)))
                   `(test ,name
                      (let (,@bindings
                            (input  (read-file-into-string ,input))
                            (output (read-file-into-string ,output)))
                        (if (%should-fail? output)
                            (signals error (parse ',rule input))
                            (finishes (parse ',rule input)))))))))
         `(progn ,@(mapcar #'test-case inputs)))))
  (frob))
