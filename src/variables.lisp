;;;; variables.lisp --- Variables used by the parser.yaml system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.yaml)

;;; Builder

(declaim (special *builder*))

(defvar *builder* 'list ; TODO nil
  "The builder object which should be used to construct the result
   objects of a successful parse.")
