(load "gunu.lisp")

(defun make-space (name prefix n)
  `(,name ,@(mapcar (lambda (i)
                      (intern (format nil "~a~a" prefix i)))
                    (loop for i from 1 to n collect i))))

(defun contract-expression (target expr &key orbital-spaces contraction-rules)
  (remove-if
   #'null
   (mapcar
    (lambda (tensor-product)

      (let ((contractions
              (find-contractions-in-product-by-target target
                                                      tensor-product
                                                      :orbital-spaces
                                                      orbital-spaces
                                                      :contraction-rules
                                                      contraction-rules)))
        (when contractions
          (list `(contractions ,contractions) tensor-product))))
    (expand-expression expr))))

(defun tensor-sum (&rest args)
  `(+ ,@(reduce (lambda (tsr rest) (ccase (car tsr)
                                     ('+ (append (cdr tsr) rest))
                                     (t (cons tsr rest))))
                args
                :from-end t
                :initial-value nil)))

(let* ((orbital-spaces
         (list (append (make-space 'H 'h 20) '(i j k l m n))
               (append (make-space 'P 'p 20) '(a b c d e f))
               (make-space 'G 'g 20)
               (append (make-space 'pq 'pq- 20) '(p q r s))))

       (partition '((PQ H P)))

       (contraction-rules
         '(((H H) 0 1)
           ((P P) 1 0)))

       ;; tensors
       vpqrs f t1 t2 r1 r2

       (r1-like '(_ (G H)))
       (r2-like '(_ (G H) (P H))))

  (setq *print-log* nil)
  (setq vpqrs (partition-tensor '(V (p q) (r s))
                                :orbital-spaces orbital-spaces
                                :partition partition))

  (setq f (partition-tensor '(f (p q))
                            :orbital-spaces orbital-spaces
                            :partition partition
                            :from-index 4))

  (setq t1 (name-legs-by-space-name '(T (P H))
                                    :orbital-spaces orbital-spaces
                                    :from-index 6))

  (setq t2 (name-legs-by-space-name '(T (P H) (P H))
                                    :orbital-spaces orbital-spaces
                                    :from-index 7))

  (setq r1 (name-legs-by-space-name '(R (G H))
                                    :orbital-spaces orbital-spaces
                                    :from-index 9))

  (setq r2 (name-legs-by-space-name '(R (G H) (P H))
                                    :orbital-spaces orbital-spaces
                                    :from-index 10))

  (format t "~&======~{~&=> ~a~}" (list vpqrs f t1 t2 r1 r2))

  (format t "~&~& DOING R1")
  (with-open-file (s "eom-ip/r1.lisp"
                      :direction :output
                      :if-exists :supersede)
    (time (format s "~s"
                  (contract-expression r1-like
                                       (list '* (tensor-sum f vpqrs)
                                             (list '+ t1 t2) (list '+ r1 r2))
                                       :orbital-spaces orbital-spaces
                                       :contraction-rules contraction-rules))))

  (format t "~&~& DOING r2")
  (with-open-file (s "eom-ip/r2.lisp"
                           :direction :output
                           :if-exists :supersede)
    (time (format s "~s"
            (contract-expression r2-like
                                 (list '* (tensor-sum f vpqrs)
                                       (list '+ t1 t2) (list '+ r1 r2))
                                 :orbital-spaces orbital-spaces
                                 :contraction-rules contraction-rules))))

  (format t "~&~&Done")

  )
