;;;; parser.yaml.asd --- System definition for the parser.yaml system.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :parser.yaml
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Provides parsing of YAML 1.2 documents."
  :depends-on  (:alexandria
                (:version :let-plus                      "0.2")

                (:version :architecture.builder-protocol "0.1")
                :esrap)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "grammar"))))
  :in-order-to ((test-op (test-op :parser.yaml/test))))

(defsystem :parser.yaml/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Unit tests for the parser.yaml system."
  :depends-on  (:alexandria
                :let-plus

                (:version :fiveam      "1.3")

                (:version :parser.yaml (:read-file-form "version-string.sexp")))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "grammar")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :parser.yaml/test))))
  (uiop:symbol-call '#:parser.yaml.test '#:run-tests))
