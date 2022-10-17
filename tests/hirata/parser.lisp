;; Parser
;; :PROPERTIES:
;; :header-args:lisp: :tangle parser.lisp :comments both
;; :END:


;; [[file:README.org::*Parser][Parser:1]]
(ql:quickload :esrap)
(defpackage :hirata
  (:use :cl :esrap))
(in-package :hirata)

(defrule space (or #\space #\tab))

(defrule number
    (+ (character-ranges (#\0 #\9)))
  (:text t))
(parse 'number #1="654981981")

(defmacro %make-index-rule (name prefix &optional suffix)
  `(defrule ,name
       ,(if suffix
            `(and ,prefix number ,suffix)
            `(and ,prefix number))
     (:function (lambda (m)
                  (intern (format nil "~{~@(~a~)~}" m))))))

(%make-index-rule hole-index "h")
(%make-index-rule ghost-hole-index "h" "*")
(%make-index-rule particle-index "p")
(%make-index-rule ghost-particle-index "p" "*")
(defrule index
    (or ghost-hole-index
        ghost-particle-index
        particle-index
        hole-index))

(parse 'hole-index "h65")
(parse 'ghost-hole-index "h65*")
(parse 'index "h65*")
(parse 'ghost-particle-index "p65*")
(parse 'particle-index "p65")

(defrule op* " * ")

(defrule index-list
    (and space (+ (and index space)))
  (:function (lambda (m) (mapcar #'car (cadr m)))))

(parse 'index-list " h3 h4 p1 p2 ")

(defrule tensor
    (and (+ (not #\space))
         " (" (or index-list " ") ")")
  (:destructure (name _ indices __)
    (declare (ignore _ __))
    `(,(coerce name 'string) ,@(typecase indices
                                 (atom nil)
                                 (t indices)))))

(parse 'tensor "e ( )")
(parse 'tensor "t ( p1 p2 h3 h4 )")

(defrule float (and (or "+ " "- " "") number "." number)
  (:text t))
(progn
  (parse 'float "2.58")
  (parse 'float "+ 2.58")
  (parse 'float "- 2.58"))

(defmacro defrule-sep-by (separator name rule &rest options)
  `(defrule ,name
       (and ,rule (* (and ,separator ,rule)))
     ,@options))

(defrule-sep-by op* tensor-product
  tensor
  (:destructure (fst rest) `(,fst
                             ,@(mapcar #'cadr rest))))

(progn

  (parse 'tensor-product
         (concatenate 'string
                      "Sum ( h3 h4 p1 p2 )"
                      " * t ( p1 p2 h3 h4 )"
                      " * v ( h3 h4 p1 p2 )"))

  (parse 'tensor-product
         (concatenate 'string
                      "Sum ( h8 h9 p7 p10 )"
                      " * t ( p7 h1 ) * t ( p5 h8 ) * t ( p6 h9 )"
                      " * t ( p10 p4 h2 h3 ) * v ( h8 h9 p7 p10 )")))

(defrule permutation
    (and "P("
         index-list
         "=>"
         index-list
         ")")
  (:destructure (_ from __ to ___)
    (declare (ignore _ __ ___))
    `(:permute ,from ,to)))

(parse 'permutation "P( p5 p6 p4 h1 h2 h3 => p4 p6 p5 h1 h2 h3 )")

(defrule permutation-list
    (and "[ "
         (+ (and float (? (and op* permutation)) " "))
         "]")
  (:destructure (_ terms __)
    (declare (ignore _ __))
    (mapcar (lambda (term)
              (if (cadr term)
                  (list (car term) (cadadr term))
                  (car term)))
            terms)))

(parse 'permutation-list
       "[ + 1.0 - 1.0 * P( p5 p6 p4 h1 h2 h3 => p4 p6 p5 h1 h2 h3 ) ]")

(parse 'permutation-list
       (concatenate 'string
                    "[ + 1.0"
                    " - 1.0 * P( p5 p6 p4 h1 h2 h3 => p4 p6 p5 h1 h2 h3 )"
                    " + 1.0 * P( p5 p6 p4 h1 h2 h3 => p4 p5 p6 h1 h2 h3 )"
                    " - 1.0 * P( p5 p6 p4 h1 h2 h3 => p5 p6 p4 h2 h1 h3 ) ]"))

(defrule statement
       (and permutation-list
            op*
            tensor-product)
  (:destructure (perms _ tensors)
    (declare (ignore _))
    (list :permutations perms
          :tensors tensors)))

(parse 'statement
       (concatenate 'string
                    "[ + 1.0"
                    " - 1.0 * P( p5 p6 p7 p8 h3 h4 h1 h2 => p5 p7 p6 p8 h3 h4 h1 h2 )"
                    " - 1.0 * P( p6 p5 p7 p8 h3 h4 h1 h2 => p6 p7 p5 p8 h3 h4 h1 h2 )"
                    " - 1.0 * P( p5 p6 p8 p7 h3 h4 h1 h2 => p5 p8 p6 p7 h3 h4 h1 h2 )"
                    " - 1.0 * P( p6 p5 p8 p7 h3 h4 h1 h2 => p6 p8 p5 p7 h3 h4 h1 h2 )"
                    " + 1.0 * P( p5 p6 p7 p8 h3 h4 h1 h2 => p7 p8 p5 p6 h3 h4 h1 h2 )"
                    " + 1.0 * P( p5 p6 p7 p8 h4 h3 h1 h2 => p5 p6 p7 p8 h2 h4 h1 h3 )"
                    " - 1.0 * P( p5 p6 p7 p8 h4 h3 h1 h2 => p5 p7 p6 p8 h2 h4 h1 h3 )"
                    " - 1.0 * P( p6 p5 p7 p8 h4 h3 h1 h2 => p6 p7 p5 p8 h2 h4 h1 h3 )"
                    " - 1.0 * P( p5 p6 p8 p7 h4 h3 h1 h2 => p5 p8 p6 p7 h2 h4 h1 h3 )"
                    " - 1.0 * P( p6 p5 p8 p7 h4 h3 h1 h2 => p6 p8 p5 p7 h2 h4 h1 h3 )"
                    " + 1.0 * P( p5 p6 p7 p8 h4 h3 h1 h2 => p7 p8 p5 p6 h2 h4 h1 h3 )"
                    " + 1.0 * P( p5 p6 p7 p8 h3 h4 h1 h2 => p5 p6 p7 p8 h2 h3 h1 h4 )"
                    " - 1.0 * P( p5 p6 p7 p8 h3 h4 h1 h2 => p5 p7 p6 p8 h2 h3 h1 h4 )"
                    " - 1.0 * P( p6 p5 p7 p8 h3 h4 h1 h2 => p6 p7 p5 p8 h2 h3 h1 h4 )"
                    " - 1.0 * P( p5 p6 p8 p7 h3 h4 h1 h2 => p5 p8 p6 p7 h2 h3 h1 h4 )"
                    " - 1.0 * P( p6 p5 p8 p7 h3 h4 h1 h2 => p6 p8 p5 p7 h2 h3 h1 h4 )"
                    " + 1.0 * P( p5 p6 p7 p8 h3 h4 h1 h2 => p7 p8 p5 p6 h2 h3 h1 h4 )"
                    " + 1.0 * P( p5 p6 p7 p8 h4 h3 h2 h1 => p5 p6 p7 p8 h1 h4 h2 h3 )"
                    " - 1.0 * P( p5 p6 p7 p8 h4 h3 h2 h1 => p5 p7 p6 p8 h1 h4 h2 h3 )"
                    " - 1.0 * P( p6 p5 p7 p8 h4 h3 h2 h1 => p6 p7 p5 p8 h1 h4 h2 h3 )"
                    " - 1.0 * P( p5 p6 p8 p7 h4 h3 h2 h1 => p5 p8 p6 p7 h1 h4 h2 h3 )"
                    " - 1.0 * P( p6 p5 p8 p7 h4 h3 h2 h1 => p6 p8 p5 p7 h1 h4 h2 h3 )"
                    " + 1.0 * P( p5 p6 p7 p8 h4 h3 h2 h1 => p7 p8 p5 p6 h1 h4 h2 h3 )"
                    " + 1.0 * P( p5 p6 p7 p8 h3 h4 h2 h1 => p5 p6 p7 p8 h1 h3 h2 h4 )"
                    " - 1.0 * P( p5 p6 p7 p8 h3 h4 h2 h1 => p5 p7 p6 p8 h1 h3 h2 h4 )"
                    " - 1.0 * P( p6 p5 p7 p8 h3 h4 h2 h1 => p6 p7 p5 p8 h1 h3 h2 h4 )"
                    " - 1.0 * P( p5 p6 p8 p7 h3 h4 h2 h1 => p5 p8 p6 p7 h1 h3 h2 h4 )"
                    " - 1.0 * P( p6 p5 p8 p7 h3 h4 h2 h1 => p6 p8 p5 p7 h1 h3 h2 h4 )"
                    " + 1.0 * P( p5 p6 p7 p8 h3 h4 h2 h1 => p7 p8 p5 p6 h1 h3 h2 h4 )"
                    " + 1.0 * P( p5 p6 p7 p8 h3 h4 h1 h2 => p5 p6 p7 p8 h1 h2 h3 h4 )"
                    " - 1.0 * P( p5 p6 p7 p8 h3 h4 h1 h2 => p5 p7 p6 p8 h1 h2 h3 h4 )"
                    " - 1.0 * P( p6 p5 p7 p8 h3 h4 h1 h2 => p6 p7 p5 p8 h1 h2 h3 h4 )"
                    " - 1.0 * P( p5 p6 p8 p7 h3 h4 h1 h2 => p5 p8 p6 p7 h1 h2 h3 h4 )"
                    " - 1.0 * P( p6 p5 p8 p7 h3 h4 h1 h2 => p6 p8 p5 p7 h1 h2 h3 h4 )"
                    " + 1.0 * P( p5 p6 p7 p8 h3 h4 h1 h2 => p7 p8 p5 p6 h1 h2 h3 h4 ) ]"
                    " * t ( p5 p6 h3 h4 ) * v ( p7 p8 h1 h2 )"))

(defun parse-outfile (path)
  (with-open-file (s path)
    (loop :for line = (read-line s nil 'eol)
          :until (eq line 'eol)
          :collect
            (parse 'statement line))))

(defun save-parse-outfile (in out)
  (with-open-file (s out :direction :output :if-exists :supersede)
    (format s "~S" (parse-outfile in))))
;; Parser:1 ends here

;; Hirata to Arponen

;; Here we create a function to try to represent hirata notation into the
;; internal form of arponen in order to compare for benchmarking:


;; [[file:README.org::*Hirata to Arponen][Hirata to Arponen:1]]

;; Hirata to Arponen:1 ends here
