;;;; parser.yaml.asd --- System definition for the parser.yaml system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.yaml-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:parser.yaml-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 1
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definition

(defsystem :parser.yaml
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Provides parsing of YAML documents."
  :depends-on  (:alexandria
                #+not-yet :split-sequence
                (:version :let-plus        "0.2")
                #+not-yet (:version :more-conditions "0.1.0")

                :esrap)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              #+not-yet (:file       "conditions")
                              (:file       "variables")
                              (:file       "protocol")
                              (:file       "grammar")
                              (:file       "list-builder"))))
  :in-order-to ((test-op (test-op :parser.yaml-test))))

(defsystem :parser.yaml-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING for details
  :description "Unit tests for the parser.yaml system."
  :depends-on  (:alexandria
                :let-plus

                (:version :fiveam      "1.1")

                (:version :parser.yaml #.(version/string)))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "grammar")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :parser.yaml-test))))
  (funcall (find-symbol "RUN-TESTS" :parser.yaml.test)))
