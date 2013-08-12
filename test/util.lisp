;;;; util.lisp --- Utilities used by the unit tests for the parser.yaml system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Note: The basic idea, structure and accompanying data files (in
;;;; the data directory) of these unit tests are based on the version
;;;; 0.9.3 of the YamlReference Haskell package.

(cl:in-package #:parser.yaml.test)

;;; Grammar for test case filenames

(defrule n-binding
    (and "n=" (+ (not #\.)))
  (:destructure (name value)
                (declare (ignore name))
                (list '*n* (parse-integer (text value)))))

(defrule c-binding
    (and "c=" (+ (not (or #\. (not character)))))
  (:destructure (name value)
                (declare (ignore name))
                (list '*c* (make-keyword (string-upcase (text value))))))

(defrule t-binding
    (and "t=" (+ (not (or #\. (not character)))))
  (:destructure (name value)
                (declare (ignore name))
                (list '*chomping-style* (make-keyword (string-upcase (text value))))))

(defrule binding
    (and #\. (or n-binding c-binding t-binding))
  (:function second))

(defrule rule-name
    (+ (not #\.))
  (:text t)
  (:function string-upcase)
  (:lambda (name)
    (intern name :parser.yaml)))

(defrule tag
    (and #\. (+ character))
  (:function second)
  (:text t))

(defrule test-case-spec
    (and rule-name (* binding) (? tag)))
