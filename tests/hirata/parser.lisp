;; Parser
;; :PROPERTIES:
;; :header-args:lisp: :tangle parser.lisp :comments both
;; :header-args:makefile+: :comments both
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

;; Hirata to Arponen conversion

;; Here we create a function to try to represent hirata notation into the
;; internal form of arponen in order to compare for benchmarking:


;; [[file:README.org::*Hirata to Arponen conversion][Hirata to Arponen conversion:1]]
(in-package :hirata)
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defrule ghost-index-number
    (and character number "*")
  (:lambda (m) (cadr m)))

(parse 'ghost-index-number "H254*")

(defun transform-ghost-index (g)
  (let ((n (parse 'ghost-index-number (symbol-name g) :junk-allowed t)))
    (if n
        (intern (format nil "G~a" n))
        g)))

(defun transform-tensor (tsr)
  (let* ((legs (cdr tsr))
         (n (length legs)))
    `(,(car tsr)
      ,@(mapcar (lambda (c)
                  (list (transform-ghost-index (nth c legs))
                        (transform-ghost-index (nth (+ c (floor n 2)) legs))))
                  (loop for c below (floor n 2) collect c)))))

(transform-tensor '("y" H5 H6 P1 P2))
(transform-tensor '("y" H5 H6* P1 P2))
(transform-tensor '("y" H5 H6 P1 P2*))
(transform-tensor '("y" H5* P1))
(transform-tensor '("y"))

(defun handle-sum (tsrs &key (base 200))
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (let ((sum (find "Sum" tsrs :key #'car :test #'string=))
        contractions)
    (if sum
        (let ((indices (mapcar (lambda (i)
                                 (cons i
                                       (lambda (new)
                                         (destructuring-bind (q n ghost)
                                             (parse '(and
                                                      character number
                                                      (? "*"))
                                                    (symbol-name i))
                                           (intern (format nil "~a~a"
                                                           (if ghost
                                                               "G"
                                                               q)
                                                           new))))))
                               (cdr sum))))
          (let ((tsrs (remove "Sum" tsrs :key #'car :test #'string=)))
            (values
             (loop :for tsr :in tsrs
                   :collect
                   (let ((it (intersection (alexandria:flatten tsr)
                                           (alexandria:flatten
                                            (mapcar #'car indices)))))
                     (if it
                         (let* ((it (car it))
                                (idx (assoc it indices)))
                           (setf indices (remove it indices
                                                 :key #'car))
                           (let ((newidx (funcall (cdr idx) (incf base))))
                             (push `(,it ,newidx) contractions)
                             (sublis `((,it . ,newidx)) tsr)))
                         tsr)))
             contractions)))
        (values tsrs contractions))))

#|
(handle-sum '(("Sum" H7) ("v" H4 H5 H7 P3) ("y" H7 H6 P1 P2)))
(handle-sum '(("Sum" H7 P3) ("v" H4 H5 H7 P3) ("y" H7 H6 P1 P 3)))
|#


(defun maybe-permute (permutation tensors)
  (typecase permutation
    (atom tensors)
    (list (let* ((indices (cdadr permutation))
                 (alist (apply #'mapcar #'cons indices)))
            (sublis alist tensors)))))

(defun perm-sign (p)
  (typecase p
    (atom p)
    (list (car p))))

(apply #'mapcar (lambda (x y) (cons x y)) '((a b) (1 5)))

(defun hirata-statement->arponen-contraction (statement)
  (declare (type list statement))
  (let ((perms (getf statement :permutations))
        (tensors (getf statement :tensors)))
    (mapcar (lambda (perm tsrs)
              (let ((permuted (maybe-permute perm tsrs))
                    (sign (perm-sign perm)))
                (multiple-value-bind (tsrs contractions) (handle-sum permuted)
                  `((contractions ,contractions)
                    ,sign ,@(mapcar #'transform-tensor tsrs)))))
            perms
            (loop :for p :in perms :collect tensors))))

(let ((stmt '(:PERMUTATIONS
              ("+ 1.0" ("- 1.0" (:PERMUTE (H4 H5 H6 P3 P1 P2) (H4 H5 H6 P2 P1 P3)))
               ("- 1.0" (:PERMUTE (H4 H5 H6 P3 P2 P1) (H4 H5 H6 P1 P2 P3)))
               ("- 1.0" (:PERMUTE (H4 H5 H6 P3 P1 P2) (H4 H6 H5 P3 P1 P2)))
               ("+ 1.0" (:PERMUTE (H4 H5 H6 P3 P1 P2) (H4 H6 H5 P2 P1 P3)))
               ("+ 1.0" (:PERMUTE (H4 H5 H6 P3 P2 P1) (H4 H6 H5 P1 P2 P3)))
               ("- 1.0" (:PERMUTE (H5 H4 H6 P3 P1 P2) (H5 H6 H4 P3 P1 P2)))
               ("+ 1.0" (:PERMUTE (H5 H4 H6 P3 P1 P2) (H5 H6 H4 P2 P1 P3)))
               ("+ 1.0" (:PERMUTE (H5 H4 H6 P3 P2 P1) (H5 H6 H4 P1 P2 P3))))
              :TENSORS (("Sum" H7) ("v" H4 H5 H7 P3) ("y" H7 H6 P1 P2)))))
  (hirata-statement->arponen-contraction stmt))

(defun hirata->arponen (infile outfile)
  (let ((statements (uiop:read-file-form infile)))
    (with-open-file (s outfile :direction :output :if-exists :supersede)
      (format s "~s" (apply #'concatenate
                            'list
                            (mapcar #'hirata-statement->arponen-contraction
                                    statements))))))

(hirata->arponen "./ccsd/ccsd_density1.lisp" "lala.lisp")
;; Hirata to Arponen conversion:1 ends here
