;;;; protocol.lisp --- Protocol functions used by the parser.yaml system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
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

;;; Builder Protocol

(defgeneric make-node (builder kind &rest args &key &allow-other-keys)
  (:documentation
   "Construct and return a node representing TODO Typical properties in ARGS are

      :name   (COMPONENT1 COMPONENT2 ...)
      :value  STRING                      ; only when KIND is :option
      :bounds (START . END)

    ."))

(defgeneric finish-node (builder node)
  (:documentation
   "TODO"))

(defgeneric relate (builder relation left right)
  (:documentation
   "Establish RELATION between nodes LEFT and RIGHTT and return the
    resulting modified LEFT node (or an appropriate fresh object).

    A typical case would RELATION being :child, LEFT being the parent
    node and RIGHT being the child node."))

;; Default behavior

(defmethod finish-node ((builder t) (node t))
  node)
