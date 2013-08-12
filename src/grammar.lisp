;;;; grammar.lisp --- Grammar used by the parser.yaml system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Note: The grammar in this file is based on the file Reference.bnf
;;;; which is a part of version 0.9.3 of the YamlReference Haskell
;;;; package. This grammar has been obtained by a mostly automated
;;;; translation from Haskell parser DSL used in the YamlReference
;;;; package to the esrap grammar DSL.

(cl:in-package #:parser.yaml)

#+later (defgrammar #:cmake
            (:use #:whitespace
                  #:literals
                  #:comments))
#+later (in-grammar #:cmake)

;;; State

;; TODO better names; refer to old names in docstrings
(deftype flow-state ()
  ":block-out - Outside block sequence.
   :block-in  - Inside block sequence.
   :flow-out  - Outside flow collection.
   :flow-in   - Inside flow collection.
   :block-key - Implicit block key.
   :flow-key  - Implicit flow key."
  ;; TODO add dashes in names?
  '(member :block-out :block-in :block-key
           :flow-out :flow-in :flow-key))

(declaim (type flow-state *c*))
(defvar *c* :flow-out ; TODO initial value?
  "TODO")

(declaim (type integer *n*))
(defvar *n* 0 ; TODO initial value?
  "TODO")

(deftype chomping-style ()
  ":strip - Remove all trailing line breaks.
   :clip  - Keep first trailing line break.
   :keep  - Keep all trailing line breaks."
  '(member :strip :keep :clip))

(declaim (type chomping-style *chomping-style*))
(defvar *chomping-style* :strip ; TODO initial value?
  "TODO")

;;; Utility functions and rules

(defun non-empty? (result)
  (not (emptyp result)))

(defun not-null? (thing)
  (not (null thing)))

(defun parse-start-of-file (text position end)
  (declare (ignore text end))
  (values nil position (if (zerop position) t "Expected start of file"))) ; TODO correct?
(defrule start-of-file #'parse-start-of-file)

(defrule end-of-file
    (! character)
  (:constant nil))

(defrule start-of-line
    (or (< 1 b-break) start-of-file)
  (:constant nil))

;;; 5.1 Character Set

(defrule c-printable
    (or #\Tab #\Newline #\Return
        (character-ranges (#\  #\~))
        #\Next-Line
        (character-ranges (#\No-break_space #\UD7FF))                 ; 8 bit
        (character-ranges (#\UE000 #\Replacement_character))          ; 16 bit
        (character-ranges (#\Linear_b_syllable_b008_a #\U0010FFFF)))) ; 32 bit

(defrule nb-json
    (or #\Tab (character-ranges (#\Space #\U0010FFFF))))

;;; 5.2 Character Encodings

(defrule c-byte-order-mark
    #\zero_width_no-break_space)

;;; 5.3 Indicator Characters [4-12]

(defrule c-sequence-entry
    #\-)

(defrule c-mapping-key
    #\?)

(defrule c-mapping-value
    #\:)

(defrule c-collect-entry
    #\,)

(defrule c-sequence-start
    #\[)

(defrule c-sequence-end
    #\])

(defrule c-mapping-start
    #\{)

(defrule c-mapping-end
    #\})

(defrule c-comment
    #\#)

(defrule c-anchor
    #\&)

(defrule c-alias
    #\*)

(defrule c-tag
    #\!)

(defrule c-literal
    #\|)

(defrule c-folded
    #\>)

(defrule c-single-quote
    "'")

(defrule c-double-quote
    #\")

(defrule c-directive
    #\%)

(defrule c-reserved
    (or #\@ #\`))

(defrule c-indicator
    (or c-sequence-entry c-mapping-key c-mapping-value c-collect-entry
        c-sequence-start c-sequence-end c-mapping-start c-mapping-end
        c-comment c-anchor c-alias c-tag c-literal c-folded
        c-single-quote c-double-quote c-directive c-reserved))

(defrule c-flow-indicator
    (or c-collect-entry c-sequence-start c-sequence-end
        c-mapping-start c-mapping-end))

;;; 5.4 Line Break Characters

(defrule b-line-feed
    #\Newline
  (:constant #\Newline))

(defrule b-carriage-return
    #\Return
  (:constant #\Return))

(defrule b-char
    (or b-line-feed b-carriage-return))

(defrule nb-char
    (and (! (or c-byte-order-mark b-char)) c-printable)
  (:function second))

(defrule b-break
    (or (and b-carriage-return b-line-feed)
        b-carriage-return
        b-line-feed))

(defrule b-as-line-feed
    b-break)

(defrule b-non-content
    b-break)

;;; 5.5 White Space Characters

(defrule s-space
    #\Space
  (:constant #\Space))

(defrule s-tab
    #\Tab
  (:constant #\Tab))

(defrule s-white
    (or s-space s-tab))

(defrule ns-char
    (and (! s-white) nb-char))

;;; 5.6 Miscellaneous Characters

(defrule ns-dec-digit
    (character-ranges (#\0 #\9)))

(defrule ns-hex-digit
    (or ns-dec-digit
        (or (character-ranges (#\A #\F)) (character-ranges (#\a #\f)))))

(defrule ns-ascii-letter
    (or (character-ranges (#\A #\Z)) (character-ranges (#\a #\z))))

(defrule ns-word-char
    (or ns-dec-digit (or ns-ascii-letter #\-)))

(defrule ns-uri-char
    (or (and #\% ns-hex-digit ns-hex-digit)
        ns-word-char
        #\# #\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\, #\_
        #\. #\! #\~ #\* #\' #\( #\) #\[ #\]))

(defrule ns-tag-char
    (and (! (and (! c-flow-indicator) c-tag)) ns-uri-char))

;;; 5.7 Escaped Characters

(defrule c-escape
    "\\")

(defrule ns-esc-null
    #\0)

(defrule ns-esc-bell
    #\a)

(defrule ns-esc-backspace
    #\b)

(defrule ns-esc-horizontal-tab
    (or #\t #\Tab))

(defrule ns-esc-line-feed
    #\n)

(defrule ns-esc-vertical-tab
    #\v)

(defrule ns-esc-form-feed
    #\f)

(defrule ns-esc-carriage-return
    #\r)

(defrule ns-esc-escape
    #\e)

(defrule ns-esc-space
    #\ )

(defrule ns-esc-double-quote
    #\")

(defrule ns-esc-slash
    #\/)

(defrule ns-esc-backslash
    "\\")

(defrule ns-esc-next-line
    #\N)

(defrule ns-esc-non-breaking-space
    #\_)

(defrule ns-esc-line-separator
    #\L)

(defrule ns-esc-paragraph-separator
    #\P)

(defrule ns-esc-8-bit
    (and #\x (* ns-hex-digit 2 2)))

(defrule ns-esc-16-bit
    (and #\u (* ns-hex-digit 4 4)))

(defrule ns-esc-32-bit
    (and #\U (* ns-hex-digit 8 8)))

(defrule c-ns-esc-char
    (and c-escape
         (or ns-esc-null ns-esc-bell ns-esc-backspace
             ns-esc-horizontal-tab ns-esc-line-feed
             ns-esc-vertical-tab ns-esc-form-feed
             ns-esc-carriage-return ns-esc-escape ns-esc-space
             ns-esc-double-quote ns-esc-slash ns-esc-backslash
             ns-esc-next-line ns-esc-non-breaking-space
             ns-esc-line-separator ns-esc-paragraph-separator
             ns-esc-8-bit ns-esc-16-bit ns-esc-32-bit)))

;;; 6.1 Indentation Spaces

;; TODO macro

(defun parse-s-indent (text position end)
  (let ((n (max *n* 0)))
    (multiple-value-call #'values
      (parse `(* s-space ,n ,n) text
             :start position :end end :junk-allowed t)
      (when (zerop n) t))))
(defrule s-indent #'parse-s-indent
  (:function length))

(let ((*n* 4))
  (parse 's-indent "    "))

(defun parse-s-indent-lt (text position end)
  (if (< *n* 1)
      (values nil nil "Current indent is negative")
      (let ((n (max (1- *n*) 0)))
        (multiple-value-call #'values
          (parse `(* s-space 0 ,n)
                 text :start position :end end :junk-allowed t)
          t))))
(defrule s-indent-lt #'parse-s-indent-lt
  (:function length))

(defun parse-s-indent-le (text position end)
  (let ((n (max *n* 0)))
    (multiple-value-call #'values
      (parse `(* s-space 0 ,n)
             text :start position :end end :junk-allowed t)
      t)))
(defrule s-indent-le #'parse-s-indent-le
  (:function length))

(parse '(and (* s-space 0 0) #\Newline) "
")

#+no (let ((*n* 4) (*c* :flow-in))
  (parse 's-indent-lt "
"))
(let ((*n* 3))
  #+no (parse 's-indent-lt "
")
  (parse 's-indent-lt "")
  (parse 's-indent-lt " ")
  (parse 's-indent-lt "  "))
(handler-case
    (let ((*n* 0))
      (parse 's-indent-lt "")
      (assert nil))
  (error ()))
(handler-case
    (let ((*n* -1))
      (parse 's-indent-lt "")
      (assert nil))
  (error ()))

;;; 6.2 Separation Spaces

(defrule s-separate-in-line
    (or (+ s-white) start-of-line)
  (:constant nil))

(parse 's-separate-in-line "
" :start 1)
(parse 's-separate-in-line "
    " :start 1)
(parse 's-separate-in-line "   ")

;;; 6.3 Line Prefixes

(defun parse-s-line-prefix (text position end)
  (multiple-value-call #'values
    (parse
     (ecase *c*
       ((:block-out :block-in) 's-block-line-prefix)
       ((:flow-out :flow-in)   's-flow-line-prefix))
     text :start position :end end :junk-allowed t)
    (when (zerop *n*) t)))
(defrule s-line-prefix
    #'parse-s-line-prefix)

(defrule s-block-line-prefix
    s-indent)

(defrule s-flow-line-prefix
    (and s-indent (? s-separate-in-line)))

;;; 6.4 Empty Lines

(defrule l-empty
    (and (or s-line-prefix s-indent-lt) b-as-line-feed))

#+no (parse 'l-empty "")

;;; 6.5 Line Folding

(defrule b-l-trimmed
    (and b-non-content (+ l-empty)))

(defrule b-as-space
    b-break)

(defrule b-l-folded
    (or b-l-trimmed b-as-space))

(let ((*n* 4) (*c* :flow-in))
  (parse '(and s-indent-lt b-as-line-feed) "
"))

(let ((*n* 4) (*c* :flow-in))
 (parse 'b-l-folded " a

" :start 2))

(defun parse-s-flow-folded (text position end)
  (let ((*c* :flow-in))
    (parse '(and (? s-separate-in-line) b-l-folded s-flow-line-prefix)
           text :start position :end end :junk-allowed t)))
(defrule s-flow-folded
    #'parse-s-flow-folded)

(let ((*n* 4))
  (parse 's-flow-folded " a

        " :start 2 :junk-allowed t))

(let ((*n* 4))
  (parse 's-flow-folded " a

        " :start 2))

(let ((*n* 4) (*c* :flow-out))
  (parse 's-flow-folded " a
        a b	c " :start 2 :junk-allowed t))

;;; 6.6 Comments

(defrule c-nb-comment-text
    (and c-comment (* nb-char))
  (:function second)
  (:text t))

(defrule b-comment
    (or b-non-content end-of-file))

(defrule s-b-comment
    (and (? (and s-separate-in-line (? c-nb-comment-text))) b-comment)
  (:function cadar))

(defrule l-comment
    (and s-separate-in-line (? c-nb-comment-text) b-comment)
  (:destructure (separate text break)
    (declare (ignore separate))
    (or text (when break '(())))))

(defrule s-l-comments
    (and (or s-b-comment start-of-line) (* (not-null? l-comment)))
  (:function flatten)) ;; TODO processing

;;; 6.7 Separation Lines

(defun parse-s-separate (text position end)
  (multiple-value-call #'values
    (parse
     (ecase *c*
       ((:block-out :block-in :flow-out :flow-in) 's-separate-lines)
       ((:block-key :flow-key)                    's-separate-in-line))
     text :start position :end end :junk-allowed t)
    t))
(defrule s-separate
    #'parse-s-separate)

(defrule s-separate-lines
    (or (and s-l-comments s-flow-line-prefix) s-separate-in-line))


(let ((*n* 4) (*c* :flow-out))
  (parse 's-separate ""))

(let ((*n* 4))
  (parse 's-separate-lines ""))

;;; 6.8 Directives

(defrule l-directive
    (and c-directive
         (or ns-yaml-directive ns-tag-directive ns-reserved-directive)
         s-l-comments)
  (:function second))

(defrule helper-directive-parameter
    (and s-separate-in-line ns-directive-parameter)
  (:function second))

(defrule ns-reserved-directive
    (and ns-directive-name (* helper-directive-parameter))
  (:destructure (name parameters)
    (reduce (curry 'relate *builder* :parameter) parameters
            :initial-value (make-node *builder* :directive
                                      :name name
                                      :kind :reserved))))

(defrule ns-directive-name
    (+ ns-char)
  (:text t))

(defrule ns-directive-parameter
    (+ ns-char)
  (:text t))

#+no (parse 'ns-reserved-directive "name value1 value2")

;;; 6.8.1 Yaml Directives

(defrule ns-yaml-directive
    (and "YAML" s-separate-in-line ns-yaml-version)
  (:destructure (directive space version)
    (declare (ignore directive space))
    (list :yaml :version version)))

(defrule version-digit
    (+ ns-dec-digit)
  (:function text)
  (:function parse-integer))

(defrule ns-yaml-version
    (and version-digit #\. version-digit)
  (:destructure (major dot minor)
    (declare (ignore dot))
    (cons major minor)))

;;; 6.8.2 Tag Directives

(defrule ns-tag-directive
    (and "TAG"
         s-separate-in-line c-tag-handle
         s-separate-in-line ns-tag-prefix)
  (:destructure (directive space1 handle space2 prefix)
    (declare (ignore directive space1 space2))
    (list :tag :handle handle :prefix prefix)))

;;; 6.8.2.1 Tag Handles

(defrule c-tag-handle
    (or c-named-tag-handle c-secondary-tag-handle
        c-primary-tag-handle))

(defrule c-primary-tag-handle
    c-tag)

(defrule c-secondary-tag-handle
    (and c-tag c-tag))

(defrule c-named-tag-handle
    (and c-tag (+ ns-word-char) c-tag))

;;; 6.8.2.2 Tag Prefixes

(defrule ns-tag-prefix
    (or c-ns-local-tag-prefix ns-global-tag-prefix)
  (:text t))

(defrule c-ns-local-tag-prefix
    (and c-tag (* ns-uri-char))
  (:function second))

(defrule ns-global-tag-prefix
    (and ns-tag-char (* ns-uri-char)))

;;; 6.9 Node Properties

(defrule c-ns-properties
    (or (and c-ns-tag-property (? (and s-separate c-ns-anchor-property)))
        (and c-ns-anchor-property (? (and s-separate c-ns-tag-property)))))

;;; 6.9.1 Node Tags

(defrule c-ns-tag-property
    (or c-verbatim-tag c-ns-shorthand-tag c-non-specific-tag))

(defrule c-verbatim-tag
    (and c-tag #\< (+ ns-uri-char) #\>)
  (:function third)
  (:text t))

(defrule c-ns-shorthand-tag
    (and c-tag-handle (+ ns-tag-char))
  (:function second)
  (:text t))

(defrule c-non-specific-tag
    c-tag)

;;; 6.9.2 Node Anchors

(defrule c-ns-anchor-property
    (and c-anchor ns-anchor-name)
  (:function second))

(defrule ns-anchor-char
    (and (! c-flow-indicator) ns-char))

(defrule ns-anchor-name
    (+ ns-anchor-char)
  (:text t))

;;; 7.1 Alias Nodes

(defrule c-ns-alias-node
    (and c-alias ns-anchor-name))

;;; 7.2 Empty Nodes

(defrule e-scalar
    (and))

(defrule e-node
    e-scalar)

;;; 7.3.1 Double Quoted Style

(defrule nb-double-char
    (or (and (and)                            c-ns-esc-char)
        (and (! (or c-double-quote c-escape)) nb-json))
  (:function second))

(defrule ns-double-char
    (and (! s-white) nb-double-char)
  (:function second))

(defrule c-double-quoted
    (and c-double-quote nb-double-text c-double-quote)
  (:function second))

(defun parse-nb-double-text (text position end)
  (parse
   (ecase *c*
     ((:flow-out :flow-in)   'nb-double-multi-line)
     ((:block-key :flow-key) 'nb-double-one-line))
   text :start position :end end :junk-allowed t))
(defrule nb-double-text
    #'parse-nb-double-text)

(defrule nb-double-one-line
    (* nb-double-char)
  (:text t))

(defun parse-s-double-escaped (text position end)
  (let ((*c* :flow-in))
    (parse '(and (* s-white) c-escape b-non-content (* l-empty) s-flow-line-prefix)
           text :start position :end end :junk-allowed t)))
(defrule s-double-escaped
    #'parse-s-double-escaped)

(defrule s-double-break
    (or s-double-escaped s-flow-folded))

(defrule nb-ns-double-in-line
    (* (and (* s-white) ns-double-char))
  (:text t))

(defrule s-double-next-line
    (and s-double-break
         (? (and ns-double-char
                 nb-ns-double-in-line
                 (or s-double-next-line (* s-white)))))
  (:function second))

(defrule nb-double-multi-line
    (and nb-ns-double-in-line (or s-double-next-line (* s-white))))

(let ((*n* 4) (*c* :flow-out))
  (parse 'nb-double-multi-line " a
        a b	c "))

(let ((*n* 4) (*c* :flow-out))
  (parse 's-flow-folded " a
        a b	c " :start 2 :junk-allowed t))

(let ((*n* 4) (*c* :flow-out))
  (parse 's-separate-in-line " a
        a b	c " :start 2 :junk-allowed t))

(let ((*n* 4) (*c* :flow-out))
  (parse 'c-double-quoted "\" a
        a b	c \""))

;;; 7.3.2 Single Quoted Style

(defrule c-quoted-quote
    (and c-single-quote #\'))

(defrule nb-single-char
    (or c-quoted-quote (and (! c-single-quote) nb-json)))

(defrule ns-single-char
    (and (! s-white) nb-single-char))

(defrule c-single-quoted
    (and c-single-quote nb-single-text c-single-quote))

(defun parse-nb-single-text (text position end)
  (parse
   (ecase *c*
     ((:flow-out :flow-in)   'nb-single-multi-line)
     ((:block-key :flow-key) 'nb-single-one-line))
   text :start position :end end :junk-allowed t))
(defrule nb-single-text
    #'parse-nb-single-text)

(defrule nb-single-one-line
    (* nb-single-char)
  (:text t))

(defrule nb-ns-single-in-line
    (* (and (* s-white) ns-single-char))
  (:text t))

(defrule s-single-next-line
    (and s-flow-folded
         (? (and ns-single-char nb-ns-single-in-line
                 (or s-single-next-line (* s-white))))))

(defrule nb-single-multi-line
    (and nb-ns-single-in-line (or s-single-next-line (* s-white))))

;;; 7.3.3 Plain Style

(defrule ns-plain-first
    (or (and (! c-indicator) ns-char)
        (and (or #\: #\? #\-) (& ns-char)))) ; spec differs in [126] here

(defun parse-ns-plain-safe (text position end)
  (parse
   (ecase *c*
     ((:flow-out :block-key) 'ns-plain-safe-out)
     ((:flow-in :flow-key)   'ns-plain-safe-in))
   text :start position :end end :junk-allowed t))
(defrule ns-plain-safe
    #'parse-ns-plain-safe)

(defrule ns-plain-safe-out
    (and (! (or c-comment c-mapping-value)) ns-char)
  (:function second))

(defrule ns-plain-safe-in
    (and (! c-flow-indicator) ns-plain-safe-out)
  (:function second))

(defrule ns-plain-char
    (or ns-plain-safe
        (and (< 1 ns-char) #\#)
        (and #\: (& ns-char)))) ; TODO handle look{ahead,behind}

(defun parse-ns-plain (text position end)
  (parse
   (ecase *c*
     ((:flow-out :flow-in)   'ns-plain-multi-line)
     ((:block-key :flow-key) 'ns-plain-one-line))
   text :start position :end end :junk-allowed t))
(defrule ns-plain
    #'parse-ns-plain)

(defrule nb-ns-plain-in-line
    (* (and (* s-white) ns-plain-char)))

(defrule ns-plain-one-line
    (and ns-plain-first nb-ns-plain-in-line)
  (:text t))

(defrule s-ns-plain-next-line
    (and s-flow-folded ns-plain-char nb-ns-plain-in-line))

(defrule ns-plain-multi-line
    (and ns-plain-one-line (* s-ns-plain-next-line)))

(let ((*c* :block-key))
  (parse 'ns-plain "language"))

(let ((*n* 4) (*c* :flow-key))
  (parse 'ns-plain "a   \" :# () b"))

(parse 'ns-plain-first ":a" :junk-allowed t)

(handler-case
    (let ((*c* :flow-in))
      (parse 'ns-plain-char " ")
      (assert nil))
  (error ()))

#+no (let ((*n* 1) (*c* :flow-key))
  (parse 'ns-plain ": "))

;;; 7.4 Flow Collection Styles

(defun in-flow (c)
  (ecase c
    ((:flow-out :flow-in)   :flow-in)
    ((:block-key :flow-key) :flow-key)))

;;; 7.4.1 Flow Sequences

(defun c-flow-sequence/helper (text position end)
  (let ((*c* (in-flow *c*)))
    (parse '(? ns-s-flow-seq-entries)
           text :start position :end end :junk-allowed t)))
(defrule c-flow-sequence
    (and c-sequence-start (? s-separate) #'c-flow-sequence/helper c-sequence-end))

(defrule ns-s-flow-seq-entries
    (and ns-flow-seq-entry (? s-separate)
         (? (and c-collect-entry (? s-separate) (? ns-s-flow-seq-entries)))))

(defrule ns-flow-seq-entry
    (or ns-flow-pair ns-flow-node))

;;; 7.4.2 Flow Mappings

(defun c-flow-mapping/helper (text position end)
  (let ((*c* (in-flow *c*)))
    (multiple-value-call #'values
      (parse '(? ns-s-flow-map-entries)
             text :start position :end end :junk-allowed t)
      t)))
(defrule c-flow-mapping
    (and c-mapping-start (? s-separate) #'c-flow-mapping/helper c-mapping-end)
  (:function third))

(defrule ns-s-flow-map-entries
    (and ns-flow-map-entry (? s-separate)
         (? (and c-collect-entry (? s-separate) (? ns-s-flow-map-entries)))))

(defrule ns-flow-map-entry
    (or (and c-mapping-key s-separate ns-flow-map-explicit-entry)
        ns-flow-map-implicit-entry))

(defrule ns-flow-map-explicit-entry
    (or ns-flow-map-implicit-entry (and e-node e-node)))

(defrule ns-flow-map-implicit-entry
    (or ns-flow-map-yaml-key-entry
        c-ns-flow-map-empty-key-entry
        c-ns-flow-map-json-key-entry))

(defrule ns-flow-map-yaml-key-entry
    (and ns-flow-yaml-node (or (and (? s-separate) c-ns-flow-map-separate-value) e-node)))

(defrule c-ns-flow-map-empty-key-entry
    (and e-node c-ns-flow-map-separate-value))

(defrule c-ns-flow-map-separate-value
    (and c-mapping-value (! ns-char) (or (and s-separate ns-flow-node) e-node)))

(defrule c-ns-flow-map-json-key-entry
    (and c-flow-json-node
         (or (and (? s-separate) c-ns-flow-map-adjacent-value)
             e-node)))

(defrule c-ns-flow-map-adjacent-value
    (and c-mapping-value
         (or (and (? s-separate) ns-flow-node)
             e-node))
  (:function second))

(defrule ns-flow-pair
    (or (and c-mapping-key s-separate ns-flow-map-explicit-entry)
        ns-flow-pair-entry))

(defrule ns-flow-pair-entry
    (or ns-flow-pair-yaml-key-entry c-ns-flow-map-empty-key-entry
        c-ns-flow-pair-json-key-entry))

(defun ns-flow-pair-yaml-key-entry/helper (text position end)
  (let ((*c* :flow-key))
    (parse 'ns-s-implicit-yaml-key text
           :start position :end end :junk-allowed t)))
(defrule ns-flow-pair-yaml-key-entry
    (and #'ns-flow-pair-yaml-key-entry/helper
         c-ns-flow-map-separate-value))

(defun c-ns-flow-pair-json-key-entry/helper (text position end)
  (let ((*c* :flow-key))
    (parse 'c-s-implicit-json-key text
           :start position :end end :junk-allowed t)))
(defrule c-ns-flow-pair-json-key-entry
    (and #'c-ns-flow-pair-json-key-entry/helper
         c-ns-flow-map-adjacent-value))

;; TODO (ns-flow-yaml-node na) where
;;
;; -- | @na@ is the \"non-applicable\" indentation value. We use Haskell's laziness
;; -- to verify it really is never used.
;; na :: Int
;; na = error "Accessing non-applicable indentation"
(defrule ns-s-implicit-yaml-key
    (and ns-flow-yaml-node (? s-separate-in-line)))

(defrule c-s-implicit-json-key
    (and c-flow-json-node (? s-separate-in-line)))

;;; 7.5 Flow Nodes

(defrule ns-flow-yaml-content
    ns-plain)

(defrule c-flow-json-content
    (or c-flow-sequence c-flow-mapping c-single-quoted c-double-quoted))

(defrule ns-flow-content
    (or ns-flow-yaml-content c-flow-json-content))

(defrule ns-flow-yaml-node
    (or c-ns-alias-node ns-flow-yaml-content
        (and c-ns-properties
             (or (and s-separate ns-flow-yaml-content)
                 e-scalar))))

(defrule c-flow-json-node
    (and (? (and c-ns-properties s-separate)) c-flow-json-content))

(defrule ns-flow-node
    (or c-ns-alias-node ns-flow-content
        (and c-ns-properties
             (or (and s-separate ns-flow-content)
                 e-scalar))))

(let ((*c* :block-key))
  (parse 'ns-flow-yaml-node ":language"))

(let ((*n* 4) (*c* :flow-in))
  (parse 'c-flow-json-node "!t \"a\""))

(let ((*n* 4) (*c* :flow-in))
  (parse 'ns-s-flow-map-entries "\"a\":b,? c,: d,e"))

(let ((*n* 4) (*c* :flow-in))
  (parse 'ns-flow-map-entry "\"a\":b"))

(let ((*c* :block-key))
  (parse 'ns-s-implicit-yaml-key "language"))
(let ((*n* 4) (*c* :flow-in))
  (parse 'c-flow-mapping "{\"a\":b,? c,: d,e}"))
(let ((*n* 4) (*c* :flow-in))
  (parse 'ns-s-flow-map-entries "\"a\":b,? c,: d,e"))

;;; 8.1.1 Block Scalar Headers

(defrule helper-c-b-block-header-1
    (and (and c-indentation-indicator c-chomping-indicator)
         (& (or s-white b-char)))
  (:function first))

(defrule helper-c-b-block-header-2
    (and c-chomping-indicator c-indentation-indicator)
 (:function rotate))

(defrule c-b-block-header
    (and (or helper-c-b-block-header-1 helper-c-b-block-header-2)
         s-b-comment)
  (:function first))

;;; 8.1.1.1 Block Indentation Indicator

(defrule indentation-digit
    (and (! #\0) ns-dec-digit)
  (:function second)
  (:function digit-char-p))

(defrule c-indentation-indicator
    (or indentation-digit detect-scalar-indentation))

(defun detect-scalar-indentation/helper (text position end)
  (let ((*c* :block-in))
    (parse '(* l-empty)
           text :start position :end end :junk-allowed t)))
(defrule detect-scalar-indentation
    (& (and (* nb-char)
            (and b-non-content (? #'detect-scalar-indentation/helper))
            count-spaces))
  (:destructure (characters stuff spaces) ; TODO simplify?
    (declare (ignore characters stuff))
    (- spaces *n*)))

(defun count-spaces/helper (spaces)
  (max 1 (+ *n* (length spaces))))
(defrule count-spaces
    (* s-space)
  (:function count-spaces/helper))

;;; 8.1.1.2 Chomping Indicator

(defrule c-chomping-indicator
    (or #\- #\+ (and))
  (:lambda (indicator)
    (case indicator
      (#\- :strip)
      (#\+ :keep)
      (t   :clip))))

(defun end-block-scalar/helper (text position end)
  (declare (ignore text end))
  (values
   (ecase *chomping-style*
     (:strip :end-scalar) ; TODO
     (:keep  :empty)
     (:clip  :end-scalar))
   position
   t))
(defrule end-block-scalar
    #'end-block-scalar/helper)

(defun b-chomped-last/helper (text position end)
  (multiple-value-call #'values
    (parse
     (ecase *chomping-style*
       (:strip 'b-non-content #+TODO-was '(and :empty-token :end-sclar b-non-content))
       (:keep  'b-as-line-feed #+TODO-was '(and b-as-line-feed :empty-token :end-scalar))
       (:clip  'b-as-line-feed))
     text :start position :end end :junk-allowed t)
    t))
(defrule b-chomped-last
    (or #'b-chomped-last/helper end-of-file))

(defun l-chomped-empty/helper (text position end)
  (multiple-value-call #'values
    (parse
     (ecase *chomping-style*
       (:strip 'l-strip-empty)
       (:keep  'l-keep-empty)
       (:clip  'l-strip-empty))
     text :start position :end end :junk-allowed t)
    t))
(defrule l-chomped-empty
    #'l-chomped-empty/helper)

(defrule l-strip-empty
    (and (* (and s-indent-le b-non-content)) (? l-trail-comments)))

(defun l-keep-empty/helper (text position end)
  (let ((*c* :block-in))
    (parse '(* l-empty) text
           :start position :end end :junk-allowed t)))
(defrule l-keep-empty
    (and #'l-keep-empty/helper (? l-trail-comments)))

(defrule l-trail-comments
    (and s-indent-lt c-nb-comment-text b-comment (* (not-null? l-comment))))

;;; 8.1.2 Literal Style

(defun c-l+literal/helper (text position end)
  (let+ (((&values (&optional indent chomping-style) position/new)
          (parse 'c-b-block-header text
                 :start position :end end :junk-allowed t)))
    (if (eql position/new position)
        (values nil position "Failed to parse block-header")
        (let ((*n*              indent #+was (+ *n* indent))
              (*chomping-style* chomping-style))
          (multiple-value-call #'values
            (parse 'l-literal-content text
                   :start (or position/new end) :end end :junk-allowed t)
            t)))))
(defrule c-l+literal
    (and c-literal #'c-l+literal/helper)
  (:function second)
  #+no (:text t))

(defun l-nb-literal-text/helper (text position end)
  (let ((*c* :block-in))
    (multiple-value-call #'values
      (parse '(* l-empty) text
             :start position :end end :junk-allowed t)
      t)))
(defrule l-nb-literal-text
    (and #'l-nb-literal-text/helper s-indent (+ nb-char))
  (:function third)
  (:text t))

(defrule b-nb-literal-next
    (and b-as-line-feed l-nb-literal-text)
  (:function second))

(defrule l-literal-content
    (and (or (and l-nb-literal-text (* b-nb-literal-next) b-chomped-last)
             end-block-scalar)
         l-chomped-empty)
  (:function first))

(let ((*n* 2))
  (parse 'c-l+literal "|-
    # afdaf
    bfdas
      #cfdsa


  #
")
  (parse 'c-l+literal "|-2
    # a
    b
      #c


  #
"))

(parse 'c-l+literal "|+
  bla")
(let ((*n* 2) (*chomping-style* :clip))
  (parse 'l-literal-content "  bla"))
(let ((*n* 2) (*chomping-style* :clip))
  (parse 'l-nb-literal-text "  bla"))

;;; 8.1.3 Folded Style

;; TODO similar to c-l+literal/helper
(defun c-l+folded/helper (text position end)
  (let+ (((&values (&optional indent chomping-style) position/new)
          (parse 'c-b-block-header text
                 :start position :end end :junk-allowed t)))
    (if (eql position/new position)
        (values nil position "Failed to parse block-header")
        (let ((*n*              (+ *n* indent))
              (*chomping-style* chomping-style))
          (parse 'l-folded-content text
                 :start (or position/new end) :end end :junk-allowed t)))))
(defrule c-l+folded
    (and c-folded #'c-l+folded/helper)
  (:function second))

#+no (let ((*n* 2))
 (parse 'c-l+folded ">2 #
     a
    b

     c

  #
"))

(defrule s-nb-folded-text
    (and s-indent ns-char (* nb-char)))

(defun l-nb-folded-lines/helper (text position end)
  (let ((*c* :block-in))
    (parse 'b-l-folded text
           :start position :end end :junk-allowed t)))
(defrule l-nb-folded-lines
    (and s-nb-folded-text (* (and #'l-nb-folded-lines/helper s-nb-folded-text))))

(defrule s-nb-spaced-text
    (and s-indent s-white (* nb-char)))

(defun b-l-spaced/helper (text position end)
  (let ((*c* :block-in))
    (multiple-value-call #'values
      (parse '(* l-empty) text
             :start position :end end :junk-allowed t)
      t)))
(defrule b-l-spaced
    (and b-as-line-feed #'b-l-spaced/helper))

(defrule l-nb-spaced
    (and s-nb-spaced-text b-l-spaced s-nb-spaced-text))

(defrule l-nb-spaced-lines
    (and s-nb-spaced-text (* (and b-l-spaced s-nb-spaced-text))))

(defun l-nb-same-lines/helper (text position end)
  (let ((*c* :block-in))
    (parse '(* l-empty) text
           :start position :end end :junk-allowed t)))
(defrule l-nb-same-lines
    (and #'l-nb-same-lines/helper (or l-nb-folded-lines l-nb-spaced-lines)))

(defrule l-nb-diff-lines
    (and l-nb-same-lines (* (and b-as-line-feed l-nb-same-lines))))

(defrule l-folded-content
    (and (or (and l-nb-diff-lines b-chomped-last)
             end-block-scalar)
         l-chomped-empty))

;;; 8.2.1 Block Sequences

(defrule detect-collection-indentation
    (& (and (* (non-empty? l-comment)) count-spaces))
  (:destructure (comments spaces)
    (declare (ignore comments))
    (- spaces *n*)))

(defrule detect-inline-indentation
    (& count-spaces))

(defun l+block-sequence/helper (text position end)
  (let+ (((&values indent position/new)
          (parse 'detect-collection-indentation text
                 :start position :end end :junk-allowed t)))
    (if (eql position/new position)
        (values nil position "Collection indentation expected")
        (let ((*n* (+ *n* indent)))
          (parse '(and s-indent c-l-block-seq-entry) text
                 :start (or position/new end) :end end :junk-allowed t)))))
(defrule l+block-sequence
    #'l+block-sequence/helper)

(defun s-l+block-indented/helper (text position end)
  (let ((*c* :block-in))
    (parse 's-l+block-indented text
           :start position :end end :junk-allowed t)))
(defrule c-l-block-seq-entry
    (and c-sequence-entry (! ns-char) #'s-l+block-indented/helper))

;; TODO similar to c-l+folded/helper
(defun s-l+block-indented/helper1 (text position end)
  (let+ (((&values indent position/new)
          (parse 'detect-inline-indentation text
                 :start position :end end :junk-allowed t)))
    (if (eql position/new position)
        (values nil position "Inline indentation expected")
        (let ((*n* (+ *n* indent 1)))
          (parse '(or ns-l-in-line-sequence ns-l-in-line-mapping) text
                 :start (or position/new end) :end end :junk-allowed t)))))
(defun s-l+block-indented/helper2 (text position end)
  (let ((*n* (1+ *n*)))
    (parse 'unparsed text :start position :end end :junk-allowed t)))
(defrule s-l+block-indented
    (or #'s-l+block-indented/helper1
        s-l+block-node
        (and e-node (? s-l-comments) #'s-l+block-indented/helper2
             #+no (recovery (+ n 1)))))

(defrule ns-l-in-line-sequence
    (and c-l-block-seq-entry (* (and s-indent c-l-block-seq-entry))))

;;; 8.2.2 Block Mappings

#+no (parse 'ns-l-block-map-entry "language: c")

(defun l+block-mapping/helper (text position end)
  (let+ (((&values indent position/new)
          (parse 'detect-collection-indentation text
                 :start position :end end :junk-allowed t)))
    (if (eql position/new position)
        (values nil position "Collection indentation expected")
        (let ((*n* (+ *n* indent)))
          (parse '(and s-indent (+ ns-l-block-map-entry)) text
                 :start (or position/new end) :end end :junk-allowed t)))))
(defrule l+block-mapping
    #'l+block-mapping/helper)

(defrule ns-l-block-map-entry
  (or c-l-block-map-explicit-entry ns-l-block-map-implicit-entry))

(defrule c-l-block-map-explicit-entry
    (and c-l-block-map-explicit-key
         (or l-block-map-explicit-value e-node)))

(defun c-l-block-map-explicit-key/helper (text position end)
  (let ((*c* :block-out))
    (parse 's-l+block-indented text
           :start position :end end :junk-allowed t)))
(defrule c-l-block-map-explicit-key
    (and c-mapping-key #'c-l-block-map-explicit-key/helper))

(defun l-block-map-explicit-value/helper (text position end)
  (let ((*c* :block-out))
    (parse 's-l+block-indented text
           :start position :end end :junk-allowed t)))
(defrule l-block-map-explicit-value
    (and s-indent c-mapping-value #'l-block-map-explicit-value/helper))

#+no (parse 'ns-l-block-map-implicit-entry "language: c")

(defrule ns-l-block-map-implicit-entry
    (and (or ns-s-block-map-implicit-key e-node)
         c-l-block-map-implicit-value))

#+no (parse 'ns-s-block-map-implicit-key "language")

(defun ns-s-block-map-implicit-key/helper (text position end)
  (let ((*c* :block-key))
    (parse '(or c-s-implicit-json-key ns-s-implicit-yaml-key) text
           :start position :end end :junk-allowed t)))
(defrule ns-s-block-map-implicit-key
    #'ns-s-block-map-implicit-key/helper)

(defun c-l-block-map-implicit-value/helper1 (text position end)
  (let ((*c* :block-out))
    (multiple-value-call #'values
      (parse 's-l+block-node text
             :start position :end end :junk-allowed t)
      t)))
(defun c-l-block-map-implicit-value/helper2 (text position end)
  (let ((*n* (1+ *n*)))
    (multiple-value-call #'values
      (parse 'unparsed text
             :start position :end end :junk-allowed t)
      t)))
(defrule c-l-block-map-implicit-value
    (and c-mapping-value
         (or #'c-l-block-map-implicit-value/helper1
             (and e-node s-l-comments #'c-l-block-map-implicit-value/helper2))))

(defrule helper-ns-l-in-line-mapping
    (and s-indent ns-l-block-map-entry)
  (:function second))

(defrule ns-l-in-line-mapping
    (and ns-l-block-map-entry helper-ns-l-in-line-mapping)
  (:destructure (entry entries)
    (finish-node
     *builder*
     (reduce (curry #'relate *builder* :entry) (cons entry entries)
             :initial-value (make-node *builder* :mapping)))))

;;; 8.2.3 Block Nodes

(defrule unparsed-line
    (and unparsed-indent (non-empty? unparsed-text) unparsed-break)
  (:function second))

(defrule unparsed
    (and (or start-of-line (and unparsed-text unparsed-break))
         (* unparsed-line))
  (:destructure (line lines)
    (finish-node *builder* (make-node *builder* :unparsed
                                      :text (if line
                                                (cons line lines)
                                                lines)))))

(defun unparsed-indent/helper (text position end)
  (let ((n (max *n* 0)))
    (multiple-value-call #'values
      (parse `(* s-space ,n ,n) text
             :start position :end end :junk-allowed t)
      (when (zerop n) t))))
(defrule unparsed-indent #'unparsed-indent/helper)

(defrule unparsed-text
    (* (not (or end-of-file c-forbidden b-break)))
  (:text t))

(defrule unparsed-break
    (or end-of-file (& c-forbidden) b-break (and)))

(defrule s-l+block-node
    (or s-l+block-in-block s-l+flow-in-block))

(defun s-l+flow-in-block/helper (text position end)
  (let ((*n* (1+ *n*))
        (*c* :flow-out))
    (parse '(and s-separate ns-flow-node s-l-comments) text
           :start position :end end :junk-allowed t)))
(defrule s-l+flow-in-block
    #'s-l+flow-in-block/helper)

(defrule s-l+block-in-block
    (or s-l+block-scalar s-l+block-collection))
;; TODO wrap in node?

(defun s-l+block-scalar/helper (text position end)
  (let ((*n* (1+ *n*)))
    (parse '(and s-separate (? (and c-ns-properties s-separate))) text
           :start position :end end :junk-allowed t)))
(defrule s-l+block-scalar
    (and #'s-l+block-scalar/helper (or c-l+literal c-l+folded)))

(let ((*n* 2) (*c* :block-in))
 (parse 's-l+block-scalar " !foo
   |
   a
"))

(defun s-l+block-collection/helper1 (text position end)
  (let ((*n* (1+ *n*)))
    (multiple-value-call #'values
      (parse '(and s-separate c-ns-properties (& s-l-comments)) text
             :start position :end end :junk-allowed t)
      t)))
(defun s-l+block-collection/helper2 (text position end)
  (let ((*n* (seq-spaces)))
    (multiple-value-call #'values
      (parse 'l+block-sequence text
             :start position :end end :junk-allowed t)
      t)))
(defrule s-l+block-collection
    (and (? #'s-l+block-collection/helper1) s-l-comments
         (or #'s-l+block-collection/helper2 l+block-mapping)))

(defun seq-spaces ()
  (ecase *c*
    (:block-out (1- *n*))
    (:block-in  *n*)))

;;; 9.1.1 Document Prefix

(defrule l-document-prefix
    (and (? c-byte-order-mark) (* (non-empty? l-comment)))
  (:destructure (mark comments)
    (when (or mark comments)
      (list mark comments))))

;;; 9.1.2 Document Markers

(defrule c-directives-end
    "---")

(defrule c-document-end
    "...")

(defrule l-document-suffix
    (and c-document-end s-l-comments))

(defrule c-forbidden
    (and start-of-line (or c-directives-end c-document-end)
         (or b-char s-white end-of-file)))

;;; 9.1.3 Bare Documents

(defun l-bare-document/helper (text position end)
  (let ((*c* :block-in)
        (*n* -1))
    (parse 's-l+block-node text
           :start position :end end :junk-allowed t)))
(defrule l-bare-document
    (and (! c-forbidden) #'l-bare-document/helper)
  (:function second))

;;; 9.1.4 Explicit Documents

;; unparsed uses the global value of *n* which is 0
(defrule l-explicit-document
    (and c-directives-end
         (or l-bare-document (and e-node (? s-l-comments) unparsed))
         #+no recovery #+no unparsed
         ))

;;; 9.1.5 Directives Documents

(defrule l-directives-document
    (and (+ l-directive) l-explicit-document))

;;; 9.2 Streams:

(defrule l-any-document
    (or l-directives-document
        l-explicit-document
        l-bare-document))

(defrule document-prefix-and-explicit-document
    (and (* (non-empty? l-document-prefix))
         (? l-explicit-document))
  (:destructure (prefix document)
    (when (or prefix document)
      (list prefix document))))

(defrule l-yaml-stream
    (and (* (non-empty? l-document-prefix))
         (or end-of-file
             (and c-document-end (& (or b-char s-white end-of-file)))
             l-any-document)
         (? (+ (or (and (+ l-document-suffix)
                        (* (non-empty? l-document-prefix))
                        (or end-of-file l-any-document))
                   (non-empty? document-prefix-and-explicit-document))))
         #+TODO-was (* (non-empty? (or (and (+ l-document-suffix)
                                            (* (non-empty? l-document-prefix))
                                            (or end-of-file l-any-document))
                                       (and (* (non-empty? l-document-prefix))
                                            (? l-explicit-document)) )))))

#+no (parse 's-l+block-node
       "language: c
compiler:
  - gcc
  - clang
# Change this to your needs
script: ./configure && make
")

#+no (parse 'l-yaml-stream
       "language: c
compiler:
  - gcc
  - clang
# Change this to your needs
script: ./configure && make
")
