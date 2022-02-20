;; todo define packages and all that
(load "gunu.lisp")

(defmacro assert-equal (left right)
  `(assert (equal ,left ,right)))

(assert-equal (cartesian-product (H P) (a b c) (1 2 3 5))
              '((H A 1) (H A 2) (H A 3) (H A 5)
                (H B 1) (H B 2) (H B 3) (H B 5)
                (H C 1) (H C 2) (H C 3) (H C 5)
                (P A 1) (P A 2) (P A 3) (P A 5)
                (P B 1) (P B 2) (P B 3) (P B 5)
                (P C 1) (P C 2) (P C 3) (P C 5)))

(assert-equal (cartesian-product (H (P)) ((a)))
              '((H (A)) ((P) (A))))

(assert-equal (all-permutations '(a b))
              '((A B) (B A)))
(assert-equal (all-permutations '(a b c))
              '((A B C) (A C B) (B C A) (B A C) (C A B) (C B A)))
(assert-equal (all-permutations '(a b c d))
              '((A B C D) (A B D C) (A C D B) (A C B D) (A D B C) (A D C B)
                (B C D A) (B C A D) (B D A C)
                (B D C A) (B A C D) (B A D C) (C D A B) (C D B A) (C A B D)
                (C A D B) (C B D A) (C B A D)
                (D A B C) (D A C B) (D B C A) (D B A C) (D C A B) (D C B A)))

(assert-equal (get-node-pairs 1) '((0 0)))
(assert-equal (get-node-pairs 2) '((0 0) (0 1) (1 1)))
(assert-equal (get-node-pairs 3) '((0 0) (0 1) (0 2) (1 1) (1 2) (2 2)))

(assert-equal (ordered-subsets-with-repetition 2 2)
              '((0 0) (0 1) (1 1)))
(assert-equal (ordered-subsets-with-repetition 2 5)
              '((0 0) (0 1) (0 2) (0 3) (0 4) (1 1) (1 2) (1 3)
                (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4)))
(assert-equal (ordered-subsets-with-repetition 3 3)
              '((0 0 0) (0 0 1) (0 0 2) (0 1 1) (0 1 2)
                (0 2 2) (1 1 1) (1 1 2) (1 2 2) (2 2 2)))
(assert-equal (ordered-subsets-with-repetition 4 4)
              '((0 0 0 0) (0 0 0 1) (0 0 0 2) (0 0 0 3) (0 0 1 1) (0 0 1 2)
                (0 0 1 3) (0 0 2 2) (0 0 2 3) (0 0 3 3) (0 1 1 1) (0 1 1 2)
                (0 1 1 3) (0 1 2 2) (0 1 2 3) (0 1 3 3) (0 2 2 2) (0 2 2 3)
                (0 2 3 3) (0 3 3 3) (1 1 1 1) (1 1 1 2) (1 1 1 3) (1 1 2 2)
                (1 1 2 3) (1 1 3 3) (1 2 2 2) (1 2 2 3) (1 2 3 3) (1 3 3 3)
                (2 2 2 2) (2 2 2 3) (2 2 3 3) (2 3 3 3) (3 3 3 3)))

(assert-equal
 (expression-to-lists '(* (v (a i))
                        (+ 1 (t (a i)) (t (a i) (b j)))
                        (+ (r (g i)) (r (g i) (a j)))))
 '(((V (A I)))
   (1 (T (A I)) (T (A I) (B J)))
   ((R (G I)) (R (G I) (A J)))))

(assert-equal
 (expand-expression '(* (v (a i))
                      (+ 1 (t (a i)) (t (a i) (b j)))
                      (+ (r (g i)) (r (g i) (a j)))))
 '(((V (A I)) 1 (R (G I)))
   ((V (A I)) 1 (R (G I) (A J)))
   ((V (A I)) (T (A I)) (R (G I)))
   ((V (A I)) (T (A I)) (R (G I) (A J)))
   ((V (A I)) (T (A I) (B J)) (R (G I)))
   ((V (A I)) (T (A I) (B J)) (R (G I) (A J)))))

(progn (assert (match-index-to-space 'k '(H i j k l)))
       (assert (not (match-index-to-space 'H '(H i j k l)))))

(progn (assert (equal (find-space-by-leg 'k '((P a b c) (H i j k l)))
                      '(H I J K L)))
       (assert (not (find-space-by-leg 'a '((H i j k l))))))

(assert-equal
 (find-space-by-name 'p '((PQ p q r s) (p a b c)))
 '(p a b c))

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

(assert-equal (stich-together '(a d)
                              '(a b) '(c d))
              '(c b))
(assert-equal (stich-together '(b c)
                              '(a b) '(c d))
              '(a d))

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

(assert-equal (get-contracted-indices
               '((contraction ((e d) (k j)))
                 (v (a b) (c d))
                 (h (e f) (g h))
                 (l (i j) (k l))) :killed-pair '(x x))
              '(((A B) (C F))
                ((X X) (G H))
                ((I L) (X X))))

(assert-equal (get-contracted-temp-tensor
               '((contraction ((e d) (k j)))
                 (v (a b) (c d))
                 (h (e f) (g h))
                 (l (i j) (k l))))
              '(contracted (A B) (C F) (G H) (I L)))

(assert-equal (get-contracted-temp-tensor
               '((contraction ((b a) (j k)))
                 (V (J I) (A B))
                 (T (C K))
                 (R (G L))))
              '(contracted (C I) (G L)))

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

(let ((vals '(((a b c) . nil)
              ((a (a) b c) . t)
              ((((a)) ((b e f g)) ((((b))))) . t))))
  (loop for (lst . val) in vals
        do (assert (eq (symbols-repeated-p lst) val))))

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

    (assert-with-env
     (find-contractions-in-product-by-target '(_ (P H))
                                             '((f (a b)) (t (c i))))
     '(((B A)) ((B C))))

    (assert-with-env
     (find-contractions-in-product-by-target '(_ (G H))
                                             '((f (a b)) (t (c i))))
     '())

    (assert-with-env
     (find-contractions-in-product-by-target '(_ (H P))
                                             '((f (a b)) (t (c i))))
     '())))

(assert-equal
 (contract-expressions-by-target '(_ (P H))
                                 '(* (+ (f (a b)) (f (i j)))
                                   (t (c k)))
                                 :orbital-spaces
                                 '((H i j k)
                                   (P a b c))
                                 :contraction-rules
                                 '(((H H) 0 1)
                                   ((P P) 1 0)))
 '(+ ((CONTRACTION ((B A))) (F (A B)) (T (C K)))
     ((CONTRACTION ((B C))) (F (A B)) (T (C K)))
     ((CONTRACTION ((I J))) (F (I J)) (T (C K)))
     ((CONTRACTION ((I K))) (F (I J)) (T (C K)))))

(assert-equal (space-subseq :orbital-spaces '((H 1 2 3 4) (P a b c) (G g g2))
                            :from-index 2)
              '((H 3 4) (P c) (G)))

(let ((vals '((0 . (t (h1 p1) (p2 h2)))
              (1 . (t (h2 p2) (p3 h3)))
              (2 . (t (h3 p3) (p4 h4))))))
  (loop for (from-index . result) in vals
        do (assert (equal
                    (name-legs-by-space-name
                     '(t (H P) (P H))
                     :orbital-spaces '((H h1 h2 h3 h4) (P p1 p2 p3 p4))
                     :from-index from-index)
                    result))))

(let ((orbital-spaces '((PQ p q r s)
                        (H i j k l)
                        (P a b c d)))
      (partition '((PQ H P))))

  (partition-tensor '(f (p q))
                    :orbital-spaces orbital-spaces
                    :partition partition)
  (partition-tensor '(V (p q) (r s))
                    :orbital-spaces orbital-spaces
                    :partition partition))

(let ((orbital-spaces '((PQ p q r s)
                        (H i j k l)
                        (P a b c d)))
      (partition '((PQ H P))))
  (latex (partition-tensor '(f (p q))
                           :orbital-spaces orbital-spaces
                           :partition partition))
  (latex (partition-tensor '(V (p q) (r s))
                    :orbital-spaces orbital-spaces
                    :partition partition)))
