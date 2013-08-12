;;;; package.lisp --- Package definition for unit tests of the parser.yaml system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.yaml.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:esrap

   #:fiveam

   #:parser.yaml)

  (:shadowing-import-from #:esrap
   #:!)

  (:import-from #:parser.yaml
   #:*c* #:*n* #:*chomping-style*)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the parser.yaml system."))

(cl:in-package #:parser.yaml.test)

(def-suite parser.yaml)

(defun run-tests ()
  (run! 'parser.yaml))
