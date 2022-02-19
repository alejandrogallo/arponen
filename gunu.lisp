(defmacro cartesian-product (&rest lists)
  (let* ((indices (loop for i from 1 to (length lists)
                        collect (gensym (format nil "~a-i-" i))))
         (initial-value `(loop for ,(car (last indices)) in ',(car (last lists))
                               collect `(,,@indices))))
    (reduce
     (lambda (x y)
       `(loop for ,(car x) in ',(cadr x)
              nconc ,y))
     (mapcar #'list (butlast indices) (butlast lists))
     :from-end t
     :initial-value initial-value)))

(assert (equal (cartesian-product (H P) (a b c) (1 2 3 5))
               '((H A 1) (H A 2) (H A 3) (H A 5) (H B 1) (H B 2) (H B 3) (H B 5)
                 (H C 1) (H C 2) (H C 3) (H C 5)
                 (P A 1) (P A 2) (P A 3) (P A 5) (P B 1) (P B 2) (P B 3) (P B 5)
                 (P C 1) (P C 2) (P C 3) (P C 5))))

(defun expression-to-lists (exp)
  (ecase (car exp)
    ('* (let ((operands
                (mapcar (lambda (e) (case (car e)
                                      ('+ (cdr e))
                                      (t (list e))))
                        (cdr exp))))
          operands))))

(assert (equal
         (expression-to-lists '(* (v (a i))
                                (+ 1 (t (a i)) (t (a i) (b j)))
                                (+ (r (g i)) (r (g i) (a j)))))
         '(((V (A I)))
           (1 (T (A I)) (T (A I) (B J)))
           ((R (G I)) (R (G I) (A J))))))

(defun expand-expression (expr)
  (eval `(cartesian-product ,@(expression-to-lists expr))))

(assert (equal
         (expand-expression '(* (v (a i))
                                (+ 1 (t (a i)) (t (a i) (b j)))
                                (+ (r (g i)) (r (g i) (a j)))))
         '(((V (A I)) 1 (R (G I)))
           ((V (A I)) 1 (R (G I) (A J)))
           ((V (A I)) (T (A I)) (R (G I)))
           ((V (A I)) (T (A I)) (R (G I) (A J)))
           ((V (A I)) (T (A I) (B J)) (R (G I)))
           ((V (A I)) (T (A I) (B J)) (R (G I) (A J))))))

(defun match-index-to-space (index orbital-space)
  (find index (cdr orbital-space)))
(progn (assert (match-index-to-space 'k '(H i j k l)))
       (assert (not (match-index-to-space 'H '(H i j k l)))))

(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst)))
                              (rest remain))))))

(defun match-target-with-tensor-1 (target tensor &key orbital-spaces)
  (assert (eq (length target) (length tensor)))
  (notany #'null
          (loop for target-tensor in (mapcar #'list (cdr target) (cdr tensor))
                collect
                (let ((spaces (mapcar (lambda (i) (find i orbital-spaces :key #'car))
                                      (car target-tensor))))
                  (assert (eq (length (car target-tensor)) (length (cadr target-tensor))))
                  (notany #'null (mapcar #'match-index-to-space
                                         (cadr target-tensor)
                                         spaces))))))

(progn
  (assert (match-target-with-tensor-1 '(V (H P) (P))
                                      '(t (i b) (a))
                                      :orbital-spaces
                                      '((H i)
                                        (P b a))))
  (assert (not (match-target-with-tensor-1 '(V (H P) (P))
                                           '(t (i b) (c)) ;; here
                                           :orbital-spaces
                                           '((H i)
                                             (P b a))))))


(defun match-target-with-tensor (target tensor &key orbital-spaces)
  "Here we check that Vaibj is equivalent to Viajb and so on always.
  This is general to all tensors.
  It works for any dimension thanks to permuting all the legs of
  the tensor."
  (let ((all-targets (mapcar (lambda (x) `(,(car target) ;; name
                                                   ,@x)) ;; feet
                             (all-permutations (cdr target)))))
    (loop for tt in all-targets
          thereis (match-target-with-tensor-1
                  tt tensor
                  :orbital-spaces orbital-spaces))))

(progn
  (assert (match-target-with-tensor '(V (H P) (P H))
                                    '(t (a i) (j b))
                                    :orbital-spaces
                                    '((H i j)
                                      (P b a))))
  (assert (not (match-target-with-tensor '(V (H P) (P H))
                                         '(t (i a) (j b))
                                         :orbital-spaces
                                         '((H i j)
                                           (P b a))))))

#|
Constraction rules should be something that tells us
which contractions are not zero.
For instance having

  (v (j b)) (t (a i))

here we can see that

  a b can contract: (P 1 0) (i.e. first position and zeroth position)
  i j can contract: (H 0 1) (i.e. zeroth position and first position)

A contraction is given by the format

  ((contraction ((a b)))
   (v (j b)
   (t (a i))))

and we can stich this contraction together to create a tensor
This is done by =contraction-to-temp-tensor=.

  ((contraction ((a b)))
   (v (j b)
   (t (a i)))) =>> (tv (j i)) which would match (_ (H H))

|#

(defun flatten-list (ls)
  (cond
    ((and (consp ls)
          (atom (car ls)))
     `(,(car ls) ,@(flatten-list (cdr ls))))
    ((and (consp ls)
          (consp (car ls)))
     `(,@(flatten-list (car ls)) ,@(flatten-list (cdr ls))))
    (t ls)))

(defun stich-together (contraction index-a index-b)
  (labels ((kill-matching (i)
                         ;; it will set x wherever the index belongs
                         ;; to the contraction
                         (subst 'x nil i
                           :test (lambda (new old)
                                   (declare (ignorable new))
                                   (intersection (list old) contraction)))))
    (let* ((pos-a (position 'x (kill-matching index-a)))
           (killed-b (kill-matching index-b))
           (pos-b (position 'x killed-b)))
      (if (eq pos-a pos-b)
          (error "You are trying to contract ~a and ~a at the same position ~a"
                 index-a index-b pos-a)
          (progn
            (setf (nth pos-a index-a) (car (delete 'x killed-b)))
            index-a)))))

(progn
  #+nil(stich-together '(a c)
                       '(a b) '(c d))
  (assert (equal (stich-together '(a d)
                                 '(a b) '(c d))
                 '(c b)))
  (assert (equal (stich-together '(b c)
                                 '(a b) '(c d))
                 '(a d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-and-replace-matching-indices
    (contraction tensor-indices &key killed-pair)
  (let* ((result (copy-tree tensor-indices))
         (all-indices (reduce (lambda (x y)
                                (concatenate 'list x y))
                              result)))
    (loop for index in all-indices
          do
             (case (length (intersection index contraction))
               (0 (continue))
               ;; self contraction
               (2 (return (subst killed-pair
                                 index
                                 result
                                 :test #'equal)))
               ;; usual contraction
               ;; x--<>---
               ;; we should find exactly ONE OTHER PLACE where this
               ;; contraction is linked by the contraction
               ;; otherwise it is an error
               (1 (let* ((matching-indices (remove-if
                                            (lambda (x) (equal x index))
                                            (remove-if-not
                                             (lambda (x)
                                               (intersection x contraction))
                                             all-indices))))
                    (format t "~&current: ~s matching: ~s through: ~s"
                            index matching-indices contraction)
                    (case (length matching-indices)
                      (0 (error "Unbound contractiong ~a with ~a"
                                index contraction))
                      (1 (let ((stiched (stich-together contraction
                                                        index
                                                        (car matching-indices))))
                           (return (subst
                                    killed-pair
                                    (car matching-indices)
                                    (subst stiched index result)))))
                      (t (error "Contraction arity(~a) error ~a contracts with ~a"
                                (length matching-indices) index matching-indices)))
                    ))))))

(macrolet ((assert-eq (index result)
             `(assert (equal (find-and-replace-matching-indices ,index
                                                                original
                                                                :killed-pair
                                                                '(x x))
                             ,result))))
  (let ((original '(((a b) (c d))
                    ((e f) (g h))
                    ((i j) (k l)))))

    (assert-eq '(e h) '(((a b) (c d))
                        ((g f) (x x))
                        ((i j) (k l))))

    (assert-eq '(k l) '(((a b) (c d))
                        ((e f) (g h))
                        ((i j) (x x))))

    (assert-eq '(e h) '(((a b) (c d))
                        ((g f) (x x))
                        ((i j) (k l))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-contracted-indices (contraction-tensor &key killed-pair)
  (assert (eq (caar contraction-tensor) 'contraction))
  (let ((contracted-indices (copy-list (mapcar #'cdr (cdr contraction-tensor))))
        (contractions (cadar contraction-tensor)))
    (loop for contraction in contractions
          do
             (setq contracted-indices
                   (find-and-replace-matching-indices
                    contraction
                    contracted-indices
                    :killed-pair killed-pair)))
    contracted-indices))

(assert (equal (get-contracted-indices
                '((contraction ((e d) (k j)))
                  (v (a b) (c d))
                  (h (e f) (g h))
                  (l (i j) (k l))) :killed-pair '(x x))
               '(((A B) (C F))
                 ((X X) (G H))
                 ((I L) (X X)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-contracted-temp-tensor (contraction-tensor)
  (let* ((killed-pair '(x x))
         (x-indices (get-contracted-indices contraction-tensor
                                           :killed-pair killed-pair))
         (flat-indices (reduce (lambda (x y) (concatenate 'list x y))
                               x-indices))
         (cleaned-indices (remove-if (lambda (x) (equal x killed-pair))
                                     flat-indices)))
    `(contracted ,@cleaned-indices)))

(assert (equal (get-contracted-temp-tensor
                '((contraction ((e d) (k j)))
                  (v (a b) (c d))
                  (h (e f) (g h))
                  (l (i j) (k l))))
               '(contracted (A B) (C F) (G H) (I L))))

(assert (equal (get-contracted-temp-tensor
                '((contraction ((b a) (j k)))
                  (V (J I) (A B))
                  (T (C K))
                  (R (G L))))
               '(contracted (C I) (G L))))

#|
In this routine magic happens.
So we have a target tensor with
  N_t operators
and some product of tensors with N_i operators each.
The number of contractions should be N_c,
so filters for the number of contractions are

  N_c = (Σ_i N_i) - N_t

If we need N_c contractions, we can get up to
N_c pairs of indices, where every index has a single
contraction. Therefore we need all ORDERED
subsets of length up to N_c

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (ftype (function (integer)) get-pairs))
(defun get-pairs (n)
  (loop for i from 0 below n
        nconcing (loop for j from i below n
                       collect `(,i ,j))))
(progn
  (assert (equal (get-pairs 1) '((0 0))))
  (assert (equal (get-pairs 2) '((0 0) (0 1) (1 1))))
  (assert (equal (get-pairs 3) '((0 0) (0 1) (0 2) (1 1) (1 2) (2 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ordered-subsets-with-repetition (n space-size)
  (let* ((vars (loop for i below n collect (gensym)))
         (deepest-level `(loop for ,(car (last vars))
                               from ,(car (last (butlast vars)))
                                 below ,space-size
                               collect `(,,@vars)))
         (init-var (gensym))
         (body (reduce (lambda (x y)
                         `(loop for ,(cadr x) from ,(car x) below ,space-size
                                nconcing ,y))
                       (butlast (mapcar #'list (append (list init-var)
                                                       (butlast vars))
                                        vars))
                       :initial-value deepest-level
                       :from-end t)))
    `(let ((,init-var 0))
       ,body)))

(progn
  (assert (equal (ordered-subsets-with-repetition 2 2)
                 '((0 0) (0 1) (1 1))))
  (assert (equal (ordered-subsets-with-repetition 2 5)
                 '((0 0) (0 1) (0 2) (0 3) (0 4) (1 1) (1 2) (1 3)
                   (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4))))
  (assert (equal (ordered-subsets-with-repetition 3 3)
                 '((0 0 0) (0 0 1) (0 0 2) (0 1 1) (0 1 2)
                   (0 2 2) (1 1 1) (1 1 2) (1 2 2) (2 2 2))))
  (assert (equal (ordered-subsets-with-repetition 4 4)
                 '((0 0 0 0) (0 0 0 1) (0 0 0 2) (0 0 0 3) (0 0 1 1) (0 0 1 2)
                   (0 0 1 3) (0 0 2 2) (0 0 2 3) (0 0 3 3) (0 1 1 1) (0 1 1 2)
                   (0 1 1 3) (0 1 2 2) (0 1 2 3) (0 1 3 3) (0 2 2 2) (0 2 2 3)
                   (0 2 3 3) (0 3 3 3) (1 1 1 1) (1 1 1 2) (1 1 1 3) (1 1 2 2)
                   (1 1 2 3) (1 1 3 3) (1 2 2 2) (1 2 2 3) (1 2 3 3) (1 3 3 3)
                   (2 2 2 2) (2 2 2 3) (2 2 3 3) (2 3 3 3) (3 3 3 3)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun orbital-space-name (index-name orbital-spaces)
  (car (find index-name orbital-spaces
             ;; this is important in order to have the same name
             ;; for spaces as for indices but not checking the space
             :test (lambda (el space) (member el (cdr space))))))
(let ((spaces '((H k l i) (P a b c) (PQ p q r s)))
      (vals '((i . h)
              (p . pq)
              (q . pq)
              (b . p))))
  (loop for (v . result) in vals
        do (assert (eq (orbital-space-name v spaces) result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compatible-contractions (node-a node-b &key
                                                orbital-spaces
                                                contraction-rules)
  (assert (eq (length node-a) 2))
  (assert (eq (length node-a) (length node-b)))
  (remove-if
   #'null
   (loop for (a b) in (eval `(cartesian-product ,node-a ,node-b))
         collect (let* ((name-pair (mapcar (lambda (x)
                                             (orbital-space-name
                                              x
                                              orbital-spaces))
                                           (list a b)))
                        (rule (find name-pair contraction-rules
                                    :test #'equal
                                    :key #'car)))
                   (when rule
                     (let ((positions (list (position a node-a)
                                            (position b node-b))))
                       (when (equal positions (cdr rule))
                         (format t "~&~8tcontraction ~a <> ~a through ~a"
                                 a b rule)
                         (list a b))))))))
;; test
(let ((spaces '((H I J K L)
                (P A B C D)
                (G G)))
      (rules '(((H H) 0 1)
               ((P P) 1 0)))
      (values '(((j i) (i a) . nil)
                ((j i) (i k) . ((j k)))
                ((a b) (c k) . ((b c)))
                ((i a) (g l) . ((i l)))
                ((i j) (k l) . ((i l)))
                ((i a) (b j) . ((i j) (a b))))))
  (loop for (a b . result) in values
        do
           (assert (equal (compatible-contractions a b
                           :orbital-spaces spaces
                           :contraction-rules rules)
                          result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun symbols-repeated-p (lst)
  (let ((symbols (flatten-list lst))
        s)
    (loop while (setq s (pop symbols))
          if (> (count s symbols) 0)
            do (return t))))

(let ((vals '(((a b c) . nil)
              ((a (a) b c) . t)
              ((((a)) ((b e f g)) ((((b))))) . t))))
  (loop for (lst . val) in vals
        do (assert (eq (symbols-repeated-p lst) val))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-contractions-in-product-by-number-of-legs
    (target tensor-list &key
                          orbital-spaces
                          contraction-rules)
  "Find contractions in a product.
   Some filters used are the number of contractions
     N-c = Sum (i) legs(product) - legs(target)
  "
  (let* ((N-c (/ (- (length (flatten-list (mapcar #'cdr tensor-list)))
                    (length (flatten-list (cdr target))))
                 2))
         (all-indices (loop for ts in (mapcar #'cdr tensor-list)
                            with ls = nil
                            do (setq ls (append ls ts))
                            finally (return ls)))
         (space-size (length all-indices))
         ;; '((1 1) (1 2) (2 2)) if length all-indices = 2
         (leg-pairs (get-pairs space-size))
         (which-pairs (eval `(ordered-subsets-with-repetition ,N-c
                                                              ,(length leg-pairs))))
         results)
    (format t "~&============")
    (format t "~&N-contractions: ~s" N-c)
    (format t "~&all indices: ~s" all-indices)
    (format t "~&all leg-pairs: ~s" leg-pairs)
    (format t "~&all combinations (of pairs) : ~s" which-pairs)
    (setq results
          (labels
              ((indexing (indices lst) (mapcar (lambda (i) (nth i lst)) indices)))
            (loop
              for pair-indices in which-pairs
              nconcing
              (block :pairs-discovery
                (tagbody
                   (let ((pairs (indexing pair-indices leg-pairs))
                         top-contractions)
                     (format t "~&combination: ~s pairs: ~s [~s]"
                             pair-indices
                             pairs (mapcar (lambda (x) (indexing x all-indices)) pairs))
                     (loop for pair in pairs
                           collect
                           (let* ((vertices (indexing pair all-indices))
                                  (conts (compatible-contractions
                                          (car vertices)
                                          (cadr vertices)
                                          :orbital-spaces orbital-spaces
                                          :contraction-rules contraction-rules)))
                             (cond
                               ((null conts) (return-from :pairs-discovery))
                               ((equal conts
                                       (intersection top-contractions conts
                                                     :test #'equal))
                                (format t "~&~30t⇐Exiting since ~a fully in ~a"
                                        conts top-contractions)
                                (return-from :pairs-discovery))
                               (t
                                (format t "~&~8tvertices: ~s" vertices)
                                (format t "~&~24t appending contractions ~s" conts)
                                (push conts top-contractions)))))

                     ;; START FILTERING
                     (return-from :pairs-discovery
                       (loop
                         for real-contraction in (eval `(cartesian-product
                                                         ,@top-contractions))
                         collect
                         (block :filter-real-contractions
                           (progn
                             ;; photons say: repeated letters must go!
                             (let ((letters (flatten-list real-contraction)))
                               (when (symbols-repeated-p letters)
                                 (return-from :filter-real-contractions)))
                             real-contraction))))
                     ))))))
    (remove-if #'null results)
    ))

(defun find-contractions-in-product-by-target
    (target tensor-list &key
                          orbital-spaces
                          contraction-rules)
  (let ((result (find-contractions-in-product-by-number-of-legs
                 target tensor-list :orbital-spaces orbital-spaces
                                    :contraction-rules contraction-rules))
        (all-indices (loop for i in (mapcar #'cdr tensor-list) appending i)))
    (format t "~&CONTRACTIONS TO CHECK: ~a" result)
    (remove-if #'null
     (loop for contraction in result
          collect
          (let* ((contraction-tensor `((contraction ,contraction)
                                       ,@(copy-list tensor-list)))
                 (contracted-tensor (get-contracted-temp-tensor
                                     contraction-tensor)))

            (format t "~&getting-temp-tensor... ~a ~a" contraction tensor-list)

            (if (match-target-with-tensor target
                                          contracted-tensor
                                          :orbital-spaces orbital-spaces)
                contraction
                nil))))))

(let ((orbital-spaces '((H I J K L h1 h2 h3)
                        (P A B C D p1 p2 p3)
                        (G g)))
      (contraction-rules '(((H H) 0 1)
                           ((P P) 1 0)))
      (|_ H P H| '(_ (G H) (P H)))
      (|P H P H| '(_ (P H) (P H)))
      (|Vhhpp * Tpphh * Tpphh| '((V (i a) (j b))
                                 (T (c k) (d l))
                                 (T (p1 h1) (p2 h2))))
      (|Vhphp * Thp * Rh| '((V (J I) (A B))
                            (T (C K))
                            (R (G L)))))
  (macrolet ((assert-with-env (fun-applied value)
               `(assert
                 (equal
                  ,(concatenate 'list fun-applied '(:orbital-spaces
                                                    orbital-spaces
                                                    :contraction-rules
                                                    contraction-rules))
                        ,value))))

    (assert-with-env
     (find-contractions-in-product-by-target |_ H P H| |Vhphp * Thp * Rh|)
     '(((B A) (J I))
       ((B C) (J I))
       ((B A) (J K))
       ((B C) (J K))
       ((B A) (J L))
       ((B C) (J L))))

    )

  )

