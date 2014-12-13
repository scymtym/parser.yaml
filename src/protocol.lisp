;;;; protocol.lisp --- Protocol functions used by the parser.yaml system.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.yaml)

;;; Parser protocol

#+later (defgeneric parse (source builder &key)
  (:documentation
   "Parse the content of SOURCE as \"cmake-like\" configuration options,
    construct a parse result using BUILDER and return it.

    Signal a `cmake-parse-error' when errors are encountered."))

;; Default behavior

#+later (define-condition-translating-method parse ((source t) (builder t) &key)
  ;; When an `esrap-error' is signaled, the innermost `parse' method
  ;; is specialized on `string'. It is therefore OK to assign to
  ;; SOURCE to the source slot of the condition.
  ((esrap:esrap-error cmake-parse-error
                      :var condition)
   :source   source
   :location (when (esrap:esrap-error-position condition)
               (esrap:esrap-error-position condition)))
  ((error cmake-parse-error)
   :source source))

#+later (defmethod parse ((source t) (builder t) &key)
  (error "~@<Cannot parse source ~A with builder ~A.~@:>"
         source builder))

#+later (defmethod parse ((source stream) (builder t) &key)
  (parse (read-stream-content-into-string source) builder))

#+later (defmethod parse ((source pathname) (builder t) &key)
  (parse (read-file-into-string source) builder))

#+later (defmethod parse ((source string) (builder t)
                  &key
                  (rule 'cmake))
  (let ((*builder* builder))
    (esrap:parse rule source)))
