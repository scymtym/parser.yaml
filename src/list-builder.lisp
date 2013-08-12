;;;; list-builder.lisp --- Construct list-based parse results.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.yaml)

(defmethod make-node ((builder (eql 'list)) (kind t) &rest args &key)
  (list* kind '() args))

(defmethod relate ((builder  (eql 'list))
                   (relation t)
                   (parent   list)
                   (child    t))
  (appendf (second parent) (list child))
  parent)
