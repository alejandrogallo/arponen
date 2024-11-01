;; [[file:readme.org::*Prolog][Prolog:3]]
;; todo define packages and all that
(load "arponen.lisp")
(in-package :arponen)

(defmacro assert-equal (left right)
  `(assert (equal ,left ,right)))

(defmacro assert! (expr &rest args)
  `(assert (not ,expr) ,@args))

(defmacro assert!-equal (left right)
  `(assert (not (equal ,left ,right))))

(defmacro assert-condition (expr condition-type &rest body)
  `(handler-case (progn ,expr
                        (assert nil))
     (,condition-type ,@body)))

(defmacro assert-errmsg (expr condition-type error-message)
  `(assert-condition ,expr ,condition-type
                     (m) (assert-equal (format nil "~a" m)
                                       ,error-message)))
;; Prolog:3 ends here

;; [[file:readme.org::*Types][Types:4]]
(assert (contraction? '((contraction ((a b) (c d))) nil)))
(assert (contraction? '((contraction nil) ((t (a i) (b j))))))
(assert (tensor? '(t (a i) (b j))))
(assert (tensor? '(1 nil)))
(assert (tensor? '(0 nil)))
;; Types:4 ends here

;; [[file:readme.org::*Cartesian product][Cartesian product:2]]
(assert-equal (cartesian-product (H P) (a b c) (1 2 3 5))
              '((H A 1) (H A 2) (H A 3) (H A 5)
                (H B 1) (H B 2) (H B 3) (H B 5)
                (H C 1) (H C 2) (H C 3) (H C 5)
                (P A 1) (P A 2) (P A 3) (P A 5)
                (P B 1) (P B 2) (P B 3) (P B 5)
                (P C 1) (P C 2) (P C 3) (P C 5)))

(assert-equal (cartesian-product (H (P)) ((a)))
              '((H (A)) ((P) (A))))
;; Cartesian product:2 ends here

;; [[file:readme.org::*Permutations][Permutations:2]]
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
;; Permutations:2 ends here

;; [[file:readme.org::*Node pairs building][Node pairs building:2]]
;; trivial examples
(assert-equal (get-node-pairs 1) '((0 0)))
(assert-equal (get-node-pairs 2) '((0 0) (0 1) (1 1)))
(assert-equal (get-node-pairs 3) '((0 0) (0 1) (0 2) (1 1) (1 2) (2 2)))

;;   2        3
;; (0 1 ||  2 3 4)
(assert-equal (get-node-pairs 5 :group-lengths '(2 3))
              '((0 2) (0 3) (0 4)
                (1 2) (1 3) (1 4)))
(assert-equal (get-node-pairs 5)
              '((0 0) (0 1) (0 2) (0 3) (0 4)
                (1 1) (1 2) (1 3) (1 4) (2 2)
                (2 3) (2 4) (3 3) (3 4) (4 4)))

;;   2        3       1       3
;; (0 1 ||  2 3 4  || 5 ||  6 7 8)
(assert-equal (get-node-pairs 9 :group-lengths '(2 3 1 3))
              '((0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8)
                (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8)
                (2 5) (2 6) (2 7) (2 8)
                (3 5) (3 6) (3 7) (3 8)
                (4 5) (4 6) (4 7) (4 8)
                (5 6) (5 7) (5 8)))

;;   V     T1    T2
;; (0 1 || 2 || 3 4)
(assert-equal (get-node-pairs 5 :group-lengths '(2 1 2))
              '((0 2) (0 3) (0 4)
                (1 2) (1 3) (1 4)
                (2 3) (2 4)))
;; Node pairs building:2 ends here

;; [[file:readme.org::*Pair combinations][Pair combinations:3]]
(assert-equal (ordered-subsets-with-repetition 1 2)
              '((0) (1)))

(assert-equal (ordered-subsets-with-repetition 2 2)
              '((0 0) (0 1) (1 1)))

(assert-equal (ordered-subsets-with-repetition 2 3)
              '((0 0) (0 1) (0 2) (1 1) (1 2) (2 2)))

(assert-equal (ordered-subsets-with-repetition 2 5)
              '((0 0) (0 1) (0 2) (0 3) (0 4) (1 1) (1 2) (1 3)
                (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4)))

(assert-equal (ordered-subsets-with-repetition 3 3)
              '((0 0 0) (0 0 1) (0 0 2) (0 1 1) (0 1 2)
                (0 2 2) (1 1 1) (1 1 2) (1 2 2) (2 2 2)))

;; here we would need 4 contractions between a set of
;; 4 pairs of nodes
(assert-equal (ordered-subsets-with-repetition 4 4)
              '((0 0 0 0) (0 0 0 1) (0 0 0 2) (0 0 0 3) (0 0 1 1) (0 0 1 2)
                (0 0 1 3) (0 0 2 2) (0 0 2 3) (0 0 3 3) (0 1 1 1) (0 1 1 2)
                (0 1 1 3) (0 1 2 2) (0 1 2 3) (0 1 3 3) (0 2 2 2) (0 2 2 3)
                (0 2 3 3) (0 3 3 3) (1 1 1 1) (1 1 1 2) (1 1 1 3) (1 1 2 2)
                (1 1 2 3) (1 1 3 3) (1 2 2 2) (1 2 2 3) (1 2 3 3) (1 3 3 3)
                (2 2 2 2) (2 2 2 3) (2 2 3 3) (2 3 3 3) (3 3 3 3)))
;; Pair combinations:3 ends here

;; [[file:readme.org::*Utils][Utils:2]]
(multiple-value-bind (expression _ )
    (macroexpand '(thread-first x (+ 5) (* 8)))
  (declare (ignorable _))
  (assert-equal '(* (+ x 5) 8)
                expression))

(multiple-value-bind (expression _ )
    (macroexpand '(thread-last x (+ 5) (* 8)))
  (declare (ignorable _))
  (assert-equal '(* 8 (+ 5 x))
                expression))
;; Utils:2 ends here

;; [[file:readme.org::*Utils][Utils:4]]
(let ((vals '(((a b c) . nil)
              ((a (a) b c) . t)
              ((((a)) ((b e f g)) ((((b))))) . t))))
  (loop for (lst . val) in vals
        do (assert (eq (symbols-repeated-p lst) val))))
;; Utils:4 ends here

;; [[file:readme.org::*Arithmetic expressions][Arithmetic expressions:2]]
(assert-equal (expr-to-lists '(* (a) (e))) '(((a) (e))))
(assert-equal (expr-to-lists '(* a b c (* d e (* e f e))))
              '((a b c d e e f e)))

(assert-equal (expr-to-lists '(+ (+ (a) (e))
                               (b)
                               (c)))
              '(((a)) ((e)) ((b)) ((c))))

(assert-equal (expr-to-lists '(+ (+ a e) (+ b c)))
              '((a) (e) (b) (c)))

(assert-equal (expr-to-lists '(* a (+ b c) (+ d (* e l))))
              '((A B D) (A B E L) (A C D) (A C E L)))


(assert-equal (expr-to-lists '(* (+ f v)
                               (+ 1 t1 t2 (* q1 q1) (* k1 k2) (* f2 f2))
                               (+ r1 r2)
                               |0>|))
              '((F 1 R1 0>)
                (F 1 R2 0>)
                (F T1 R1 0>)
                (F T1 R2 0>)
                (F T2 R1 0>)
                (F T2 R2 0>)
                (F Q1 Q1 R1 0>)
                (F Q1 Q1 R2 0>)
                (F K1 K2 R1 0>)
                (F K1 K2 R2 0>)
                (F F2 F2 R1 0>)
                (F F2 F2 R2 0>)
                (V 1 R1 0>)
                (V 1 R2 0>)
                (V T1 R1 0>)
                (V T1 R2 0>)
                (V T2 R1 0>)
                (V T2 R2 0>)
                (V Q1 Q1 R1 0>)
                (V Q1 Q1 R2 0>)
                (V K1 K2 R1 0>)
                (V K1 K2 R2 0>)
                (V F2 F2 R1 0>)
                (V F2 F2 R2 0>)))


(assert-equal (expr-to-lists (expr-power 2 '(+ a b)))
              '((A A) (A B) (B A) (B B)))

(assert-equal (expr-to-lists (expr-power 3 '(+ a b)))
              '((A A A) (A A B) (A B A) (A B B)
                (B A A) (B A B) (B B A) (B B B)))

(assert-equal (expr-to-lists (expr-power 4 '(+ a b)))
              '((A A A A) (A A A B) (A A B A) (A A B B) (A B A A)
                (A B A B) (A B B A) (A B B B) (B A A A) (B A A B) (B A B A)
                (B A B B) (B B A A) (B B A B) (B B B A) (B B B B)))

(assert-equal
 (expr-to-lists
  '(* (+ (fab) (fij) (fai) (fia) (vpqrs) (v...))
    (+ (1) (t1) (t2) (* (t1) (t1)) (* (t1) (t2)) (* (t2) (t2)))
    (+ (r1) (r2))))

 '(((FAB) (1) (R1)) ((FAB) (1) (R2)) ((FAB) (T1) (R1)) ((FAB) (T1) (R2))
   ((FAB) (T2) (R1)) ((FAB) (T2) (R2)) ((FAB) (T1) (T1) (R1))
   ((FAB) (T1) (T1) (R2))
   ((FAB) (T1) (T2) (R1)) ((FAB) (T1) (T2) (R2))
   ((FAB) (T2) (T2) (R1)) ((FAB) (T2) (T2) (R2))
   ((FIJ) (1) (R1))
   ((FIJ) (1) (R2))
   ((FIJ) (T1) (R1))
   ((FIJ) (T1) (R2))
   ((FIJ) (T2) (R1))
   ((FIJ) (T2) (R2))
   ((FIJ) (T1) (T1) (R1))
   ((FIJ) (T1) (T1) (R2))
   ((FIJ) (T1) (T2) (R1))
   ((FIJ) (T1) (T2) (R2))
   ((FIJ) (T2) (T2) (R1))
   ((FIJ) (T2) (T2) (R2))
   ((FAI) (1) (R1))
   ((FAI) (1) (R2))
   ((FAI) (T1) (R1))
   ((FAI) (T1) (R2))
   ((FAI) (T2) (R1))
   ((FAI) (T2) (R2))
   ((FAI) (T1) (T1) (R1))
   ((FAI) (T1) (T1) (R2))
   ((FAI) (T1) (T2) (R1))
   ((FAI) (T1) (T2) (R2))
   ((FAI) (T2) (T2) (R1))
   ((FAI) (T2) (T2) (R2))
   ((FIA) (1) (R1))
   ((FIA) (1) (R2))
   ((FIA) (T1) (R1))
   ((FIA) (T1) (R2))
   ((FIA) (T2) (R1))
   ((FIA) (T2) (R2))
   ((FIA) (T1) (T1) (R1))
   ((FIA) (T1) (T1) (R2))
   ((FIA) (T1) (T2) (R1))
   ((FIA) (T1) (T2) (R2))
   ((FIA) (T2) (T2) (R1))
   ((FIA) (T2) (T2) (R2))
   ((VPQRS) (1) (R1))
   ((VPQRS) (1) (R2))
   ((VPQRS) (T1) (R1))
   ((VPQRS) (T1) (R2))
   ((VPQRS) (T2) (R1))
   ((VPQRS) (T2) (R2))
   ((VPQRS) (T1) (T1) (R1))
   ((VPQRS) (T1) (T1) (R2))
   ((VPQRS) (T1) (T2) (R1))
   ((VPQRS) (T1) (T2) (R2))
   ((VPQRS) (T2) (T2) (R1))
   ((VPQRS) (T2) (T2) (R2))
   ((V...) (1) (R1))
   ((V...) (1) (R2))
   ((V...) (T1) (R1))
   ((V...) (T1) (R2))
   ((V...) (T2) (R1))
   ((V...) (T2) (R2))
   ((V...) (T1) (T1) (R1))
   ((V...) (T1) (T1) (R2))
   ((V...) (T1) (T2) (R1))
   ((V...) (T1) (T2) (R2))
   ((V...) (T2) (T2) (R1))
   ((V...) (T2) (T2) (R2))))

(assert-equal (expr-to-lists '(* (+ (T1 (P6 H6))) (+ (T1 (P5 H5)))))
              '(((T1 (P6 H6)) (T1 (P5 H5)))))

(assert-equal
 (expr-to-lists '(+ 1 (T1 (P6 H6))
                        (T2 (P3 H3) (P4 H4))
                        (* (+ (T1 (P6 H6))) (+ (T1 (P5 H5))))
                        (* (+ (T1 (P6 H6))) (+ (T2 (P3 H3) (P4 H4))))
                        (* (+ (T2 (P3 H3) (P4 H4))) (+ (T2 (P1 H1) (P2 H2))))))

 '((1)
   ((T1 (P6 H6)))
   ((T2 (P3 H3) (P4 H4)))
   ((T1 (P6 H6)) (T1 (P5 H5)))
   ((T1 (P6 H6)) (T2 (P3 H3) (P4 H4)))
   ((T2 (P3 H3) (P4 H4)) (T2 (P1 H1) (P2 H2)))))
;; Arithmetic expressions:2 ends here

;; [[file:readme.org::*Index spaces][Index spaces:2]]
(progn (assert (match-index-to-space 'k '(H i j k l)))
       (assert (not (match-index-to-space 'H '(H i j k l)))))
;; Index spaces:2 ends here

;; [[file:readme.org::*Index spaces][Index spaces:4]]
(progn (assert (equal (find-space-by-leg 'k '((P a b c) (H i j k l)))
                      '(H I J K L)))
       (assert (not (find-space-by-leg 'a '((H i j k l))))))
;; Index spaces:4 ends here

;; [[file:readme.org::*Index spaces][Index spaces:6]]
(assert-equal
 (find-space-by-name 'p '((PQ p q r s) (p a b c)))
 '(p a b c))
;; Index spaces:6 ends here

;; [[file:readme.org::*Index spaces][Index spaces:7]]
(let ((spaces '((H k l i) (P a b c) (PQ p q r s)))
      (vals '((i . h)
              (p . pq)
              (q . pq)
              (b . p))))
  (loop for (v . result) in vals
        do (assert (eq (find-space-name-by-leg v spaces) result))))
;; Index spaces:7 ends here

;; [[file:readme.org::*Index spaces][Index spaces:9]]
(assert-equal (tensor-to-description '(V (i k) (l a))
                                     :orbital-spaces
                                     '((H i j k l) (P a b c d)))
              '(V (H H) (H P)))
;; Index spaces:9 ends here

;; [[file:readme.org::*Tensor sum][Tensor sum:2]]
(assert-equal (tensor-sum '(T (A b) (c d)))
              '(+ (T (a b) (c d))))
(assert-equal (tensor-sum '(T (A b) (c d)) '(V (e i)))
              '(+ (T (a b) (c d))
                  (V (e i))))
(assert-equal (tensor-sum '(* (t (a b) (c d)) (f (k l))) '(v (e i)))
              '(+ (* (T (A B) (C D)) (F (K L)))
                  (V (E I))))
(assert-equal (tensor-sum '(+ a b c d (* e d)) '(h1 h2))
              '(+ A B C D (* E D) (H1 H2)))
;; this one is very useful
(assert-equal (tensor-sum '(+ a b c d) '(+ e d) '(+ h1 h2))
              '(+ A B C D E D H1 H2))
;; Tensor sum:2 ends here

;; [[file:readme.org::*Tensor matching][Tensor matching:2]]
(assert (match-target-with-tensor-1 '(V (H P) (P))
                                    '(t (i b) (a))
                                    :orbital-spaces
                                    '((H i)
                                      (P b a))))
(assert (not (match-target-with-tensor-1 '(V (H P) (P))
                                         '(t (i b) (c)) ;; here
                                         :orbital-spaces
                                         '((H i)
                                           (P b a)))))

(assert (not (match-target-with-tensor-1 '(V (H P))
                                         '(t (i b) (c)) ;; here
                                         :orbital-spaces
                                         '((H i)
                                           (P b a)))))
;; Tensor matching:2 ends here

;; [[file:readme.org::*Tensor matching][Tensor matching:4]]
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
;; Tensor matching:4 ends here

;; [[file:readme.org::*Node symmetry][Node symmetry:3]]
(assert-equal (apply-symmetry-to-nodes '((P . Q) (S . R))
                                       '((P S) (Q R)))
              ;;
              '((Q R) (P S)))

(assert-equal (apply-symmetry-to-nodes '((a . b) (i . j))
                                       '(T (a i) (b j) (c k)))
              ;;
              '(T (B J) (A I) (C K)))

(let ((contraction '((contraction (P2 P5) (H2 H3) (H1 H4) (P1 P3))
                     (V (h1 p1) (h2 p2))
                     (T (p3 h3) (p4 h4))
                     (T (p5 h5)))))
  (destructuring-bind ((cts _ a b c) v tabij tai . nil) (apply-symmetry-to-nodes
                                               '((p3 . p4) (h3 . h4))
                                               contraction)
    (assert-equal tabij '(T (p4 h4) (p3 h3)))
    (assert-equal tai '(t (p5 h5)))
    (assert-equal v '(V (h1 p1) (h2 p2)))
    (assert-equal (list a b c) '((h2 h4) (h1 h3) (p1 p4)))))


(assert-equal (apply-symmetry-to-nodes '((p . q))
                                       '(V (P s) (q r)))
              ;;
              '(V (Q S) (P R)))



(assert-equal (apply-symmetries-to-nodes '(((p . q) (s . r)) ((p . s)) ((r . q)))
                                         '(V (P s) (q r)))
              '((V (Q R) (P S))
                (V (S P) (Q R))
                (V (P S) (R Q))))
;; Node symmetry:3 ends here

;; [[file:readme.org::*Node symmetry][Node symmetry:5]]
;; utility function
(assert-equal (triangle-pairs 1) nil)
(assert-equal (triangle-pairs 2) '((0 1)))
(assert-equal (triangle-pairs 3) '((0 1) (0 2) (1 2)))

;; fail gracefully for one dimensional diagrams
(assert! (make-node-symmetry '((p q))))

(assert-equal (make-node-symmetry '((p s) (q r)))
              '(((P . Q) (S . R))))


(assert-equal (make-node-symmetry '((p0 h0) (p1 h1) (p2 h2)))
              '(;; node 1 <> node 2
                ((P1 . P2) (H1 . H2))
                ;; node 1 <> 0 && 2 <> 1 && 0 <> 2
                ((P1 . P0) (H1 . H0) (P2 . P1) (H2 . H1) (P0 . P2) (H0 . H2))
                ;; node 0 <> node 1
                ((P0 . P1) (H0 . H1))
                ;; node 2 <> 0 && 0 <> 1 && 1 <> 2
                ((P2 . P0) (H2 . H0) (P0 . P1) (H0 . H1) (P1 . P2) (H1 . H2))
                ;; node 0 <> node 2
                ((P0 . P2) (H0 . H2))))

(let ((result '(((P2 . P3) (H2 . H3)) ;; 2 <> 3
                ((P2 . P1) (H2 . H1) (P3 . P2) (H3 . H2) (P1 . P3) (H1 . H3))
                ((P1 . P2) (H1 . H2)) ;; 1 <> 2
                ((P3 . P1) (H3 . H1) (P1 . P2) (H1 . H2) (P2 . P3) (H2 . H3))
                ((P1 . P3) (H1 . H3)) ;; 1 <> 3
                ((P1 . P0) (H1 . H0) (P2 . P1)
                 (H2 . H1) (P3 . P2) (H3 . H2)
                 (P0 . P3) (H0 . H3))
                ((P1 . P0) (H1 . H0) (P2 . P1)
                 (H2 . H1) (P0 . P2) (H0 . H2))
                ((P1 . P0) (H1 . H0) (P3 . P1)
                 (H3 . H1) (P0 . P2) (H0 . H2)
                 (P2 . P3) (H2 . H3))
                ((P1 . P0) (H1 . H0) (P3 . P1)
                 (H3 . H1) (P0 . P3) (H0 . H3))
                ((P0 . P1) (H0 . H1)) ;; 0 <> 1
                ((P0 . P1) (H0 . H1) (P2 . P3) (H2 . H3))
                ((P0 . P2) (H0 . H2) (P1 . P3) (H1 . H3))
                ((P2 . P0) (H2 . H0) (P3 . P1)
                 (H3 . H1) (P1 . P2) (H1 . H2)
                 (P0 . P3) (H0 . H3))
                ((P2 . P0) (H2 . H0) (P0 . P1)
                 (H0 . H1) (P1 . P2) (H1 . H2))
                ((P2 . P0) (H2 . H0) (P0 . P1)
                 (H0 . H1) (P3 . P2) (H3 . H2)
                 (P1 . P3) (H1 . H3))
                ((P2 . P0) (H2 . H0) (P3 . P2)
                 (H3 . H2) (P0 . P3) (H0 . H3))
                ((P0 . P2) (H0 . H2)) ;; 0 <> 2
                ((P3 . P0) (H3 . H0) (P0 . P1)
                 (H0 . H1) (P1 . P2) (H1 . H2)
                 (P2 . P3) (H2 . H3))
                ((P3 . P0) (H3 . H0) (P0 . P1)
                 (H0 . H1) (P1 . P3) (H1 . H3))
                ((P0 . P3) (H0 . H3)) ;; 0 <> 3
                ((P3 . P0) (H3 . H0) (P0 . P2)
                 (H0 . H2) (P2 . P3) (H2 . H3))
                ((P3 . P0) (H3 . H0) (P2 . P1)
                 (H2 . H1) (P0 . P2) (H0 . H2)
                 (P1 . P3) (H1 . H3))
                ((P1 . P2) (H1 . H2) (P0 . P3) (H0 . H3)))))
  (assert-equal (make-node-symmetry '((p0 h0) (p1 h1) (p2 h2) (p3 h3)))
                result)
  (assert-equal (length result) (- (* 4 3 2) 1)))
;; Node symmetry:5 ends here

;; [[file:readme.org::*Node symmetry][Node symmetry:7]]
(assert-equal (find-effective-nodes-list
               '((V (p q) (r s)) (T2 (a b) (c d))))
              '(((p q) (r s)) ((a b) (c d))))

(assert-equal (find-effective-nodes-list '((V (p q) (r s))
                                           (T2 (a b) (c d)) (T2 (a2 b2) (c2 d2))
                                           (R2 (g h) (h2 g2))))
              '(((P Q) (R S))
                ((A B) (C D) (A2 B2) (C2 D2))
                ((G H) (H2 G2))))

(assert-equal (find-effective-nodes-list '((V (p r) (q s))
                                           (T1 (a i)) (T1 (aa ii))
                                           (R1 (g e))))
              '(((p r) (q s))
                ((a i) (aa ii))
                ((g e))))

(let* ((tensors '((V (h1 p1) (h2 p2))
                  (T2 (p3 h3) (p4 h4))
                  (T1 (p5 h5))))
       (symmetries (make-symmetries-in-effective-node-list
                    tensors #'make-node-symmetry)))
  (assert-equal symmetries
                '(((H1 . H2) (P1 . P2))
                  ((P3 . P4) (H3 . H4)))))
;; Node symmetry:7 ends here

;; [[file:readme.org::*Antisymmetry][Antisymmetry:2]]
;; utility function
(assert-equal (unzip '((a b) (c d)))
              '((a c) (b d)))
(assert-equal (unzip '((a b) (c d) (e f)))
              '((a c e) (b d f)))


(assert-equal (make-antisymmetry-symmetry '((a i) (b j)))
              '(((A . B)) ((I . J))))
(assert-equal (make-antisymmetry-symmetry '((a i) (b j) (c k)))
              '(((A . B)) ((A . C)) ((B . C))
                ((I . J)) ((I . K)) ((J . K))))

(arponen::assert-equal
 (arponen::make-symmetries-in-node-list '(((p2 h2) (h3 p3)) ((p1 h1) (p4 h4)))
                                     #'arponen::make-antisymmetry-symmetry)
 '(((H2 . P3)) ((P2 . H3)) ((P1 . P4)) ((H1 . H4))))
;; Antisymmetry:2 ends here

;; [[file:readme.org::*Filtering contractions through symmetries][Filtering contractions through symmetries:2]]
(assert-equal (find-duplicate-set '#1=((a . b) (c . d))
                                  '(((c . e) (a . b))
                                    ((c . d) (a . b))
                                    #1#))
              '((c . d) (a . b)))

;; it should also accept transposed variations of the
;; elements
(assert-equal (find-duplicate-pair-set '((a . b) (d . c))
                         '(((c . e) (a . b))
                           ((c . d) (a . b))))
              '((c . d) (a . b)))

(assert-equal (find-duplicate-pair-set '((a . b) (d . c))
                         '(((c . e) (a . b))
                           ((c . d) (b . a))))
              '((c . d) (b . a)))
;; Filtering contractions through symmetries:2 ends here

;; [[file:readme.org::*Mergin nodes][Mergin nodes:2]]
(assert-equal (stich-together '(a d)
                              '(a b) '(c d))
              '(c b))
(assert-equal (stich-together '(b c)
                              '(a b) '(c d))
              '(a d))

(assert-errmsg (stich-together '(a c) '(a d) '(e f))
               simple-error
               "The contraction (A C) does not link nodes (A D) and (E F)")

(assert-errmsg (stich-together '(e c) '(a d) '(e f))
               simple-error
               "The contraction (E C) does not link nodes (A D) and (E F)")
;; Mergin nodes:2 ends here

;; [[file:readme.org::*Mergin nodes][Mergin nodes:4]]
(macrolet ((assert-eq (index result)
             `(assert (equal (find-and-replace-matching-nodes ,index
                                                                original
                                                                :killed-pair
                                                                '(x x))
                             ,result))))
  (let ((original '(((a b) (c d))
                    ((e f) (g h))
                    ((i j) (k l) (h1 h2)))))

    ;; 0-1 contraction
    (assert-eq '(e h) '(((a b) (c d))
                        ((g f) (x x))
                        ((i j) (k l) (h1 h2))))

    ;; self contraction
    (assert-eq '(k l) '(((a b) (c d))
                        ((e f) (g h))
                        ((i j) (x x) (h1 h2))))

    ;; 1-0 contraction
    (assert-eq '(b k) '(((a l) (c d))
                        ((e f) (g h))
                        ((i j) (X X) (h1 h2))))

    ;; contraction with tripes
    (assert-eq '(a h2) '(((h1 b) (c d))
                         ((e f) (g h))
                         ((i j) (k l) (x x))))

    ;; contraction within the tensor
    (assert-eq '(a d) '(((c b) (X X))
                        ((e f) (g h))
                        ((i j) (k l) (h1 h2))))

    ;; todo: test error messages

    ))
;; Mergin nodes:4 ends here

;; [[file:readme.org::*Mergin nodes][Mergin nodes:6]]
(assert-equal (get-contracted-nodes
               '((contraction ((e d) (k j)))
                 (v (a b) (c d))
                 (h (e f) (g h))
                 (l (i j) (k l))) :killed-pair '(x x))
              '(((A B) (C F))
                ((X X) (G H))
                ((I L) (X X))))
;; Mergin nodes:6 ends here

;; [[file:readme.org::*Effective temporary tensor][Effective temporary tensor:2]]
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
                 (R (G L))) :name '|v*t*r|)
              '(|v*t*r| (C I) (G L)))

(assert-equal (get-contracted-temp-tensor
               '((contraction nil)
                 (F (a i))) :name '|Fai|)
              '(|Fai| (A I)))
;; Effective temporary tensor:2 ends here

;; [[file:readme.org::*Compatible contractions][Compatible contractions:2]]
;; test
(let ((spaces '((H I J K L)
                (P A B C D)
                (G G))))

  (let ((rules '(((H H) 0 1)
                 ((P P) 1 0)))
        (values '(((j i) (i a) . nil)
                  ((j i) (i k) . ((j k)))
                  ((a b) (c k) . ((b c)))
                  ((i a) (g l) . ((i l)))
                  ((i j) (k l) . ((i l)))
                  ((i a) (b j) . ((i j) (a b)))
                  ((a i) (j b) . nil)
                  ((a b) (c d) . ((b c))))))
    (loop for (a b . result) in values
          do (assert (equal (compatible-contractions a b
                                                     :orbital-spaces spaces
                                                     :contraction-rules rules)
                            result))))

  (let ((spaces '((H I J K L)
                  (P A B C D)
                  (G G)))
        ;; test with some absurd contraction rules
        (rules '(((H H) 0 1)
                 ((H P) 1 1)
                 ((P H) 0 1)
                 ((P G) 0 0)
                 ((P P) 1 0)))
        (values '(((j i) (i a) . ((i a)))
                  ((j i) (i k) . ((j k)))
                  ((a b) (c k) . ((a k) (b c)))
                  ((a i) (g l) . ((a l) (a g)))
                  ((i j) (k l) . ((i l)))
                  ((i a) (b j) . ((i j) (a b))))))
    (loop for (a b . result) in values
          do (assert (equal (compatible-contractions a b
                                                     :orbital-spaces spaces
                                                     :contraction-rules rules)
                            result)))))
;; Compatible contractions:2 ends here

;; [[file:readme.org::*Checking for connectedness][Checking for connectedness:2]]
(macrolet ((! (&rest pts)
             `(mapcar (lambda (p)
                        (position p node-pairs :test #'equal)) ',pts)))
  (let ((node-pairs
          '((0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8) ;; | 1st -> all
            (1 4) (1 5) (1 6) (1 7) (1 8)    ;; | 2nd diagram -> 3
            (2 4) (2 5) (2 6) (2 7) (2 8)    ;; |
            (3 4) (3 5) (3 6) (3 7) (3 8)))) ;; |

    ;; this contraction only goes from the first diagram to the second
    (assert! (is-connected-contraction (! (0 1) (0 2) (0 3))
                                       node-pairs :group-lengths '(1 3 5)))

    ;; this contraction only goes from the 2nd diagram to the 3rc
    (assert! (is-connected-contraction (! (1 4) (1 6) (3 4) (3 7) (2 6))
                                       node-pairs :group-lengths '(1 3 5)))

    ;; this is quick, it just goes to from 1 to 2 and to 3 directly
    (assert (is-connected-contraction (! (0 1) (2 5))
                                      node-pairs :group-lengths '(1 3 5)))

    ;; this is less quick, it goes from 1 to 2 twice and then goes to 3
    (assert (is-connected-contraction (! (0 1) (0 2) (2 5))
                                      node-pairs :group-lengths '(1 3 5)))))
;; Checking for connectedness:2 ends here

;; [[file:readme.org::*Case study: Vijab with T1 and T2 coupling to singles excitations][Case study: Vijab with T1 and T2 coupling to singles excitations:1]]
(let ((orbital-spaces '((H i j k l m n o h1 h2 h3 h4 h5)
                        (P a b c d e f g p1 p2 p3 p4 p5)))
      (contraction-rules '(((H H) 0 1)
                           ((P P) 1 0))))
  (labels ((with-rules (target tensor)
             (find-contractions-in-product-by-number-of-legs target tensor
                                                             :orbital-spaces
                                                             orbital-spaces
                                                             :contraction-rules
                                                             contraction-rules))
           (with-rules-c (target tensor) (let ((*only-connected-diagrams* t)
                                               (*allow-self-contractions* nil))
                                           (with-rules target tensor))))
    (let ((*filter-node-symmetry* t))
      (with-rules-c '(_ (P H) (P H))
        '((V (h1 p1) (h2 p2))
          (T2 (p3 h3) (p4 h4))
          (T1 (p5 h5)))))
    ))

#+(or)
'((1 (P2 P5) (H2 H4) (H1 H3) (P1 P3))
  (2 (H2 H5) (P2 P4) (H1 H3) (P1 P3))
  (3 (H2 H5) (P2 P5) (H1 H3) (P1 P3))
  (4 (P2 P5) (H2 H3) (H1 H4) (P1 P3))
  (5 (H2 H5) (P2 P3) (P1 P4) (H1 H3))
  (6 (P2 P5) (H2 H4) (P1 P4) (H1 H3))
  (7 (H2 H5) (P2 P4) (H1 H4) (P1 P3))
  (8 (H2 H5) (P2 P5) (P1 P4) (H1 H3))
  (9 (H2 H5) (P2 P5) (H1 H4) (P1 P3))
  (10 (P2 P4) (H2 H3) (H1 H5) (P1 P3))
  (11 (H2 H4) (P2 P3) (P1 P5) (H1 H3))
  (12 (P2 P5) (H2 H3) (H1 H5) (P1 P3))
  (13 (H2 H5) (P2 P3) (P1 P5) (H1 H3))
  (14 (H2 H4) (P2 P4) (P1 P5) (H1 H3))
  (15 (H2 H4) (P2 P4) (H1 H5) (P1 P3))
  (16 (P2 P5) (H2 H4) (H1 H5) (P1 P3))
  (17 (H2 H5) (P2 P4) (P1 P5) (H1 H3))
  (18 (P2 P5) (H2 H3) (H1 H4) (P1 P4))
  (19 (H2 H5) (P2 P3) (H1 H4) (P1 P4))
  (20 (H2 H5) (P2 P5) (H1 H4) (P1 P4))
  (21 (H2 H3) (P2 P3) (P1 P5) (H1 H4))
  (22 (H2 H3) (P2 P3) (H1 H5) (P1 P4))
  (23 (P2 P4) (H2 H3) (P1 P5) (H1 H4))
  (24 (H2 H4) (P2 P3) (H1 H5) (P1 P4))
  (25 (P2 P5) (H2 H3) (H1 H5) (P1 P4))
  (26 (H2 H5) (P2 P3) (P1 P5) (H1 H4))
  (27 (P2 P5) (H2 H4) (H1 H5) (P1 P4))
  (28 (H2 H5) (P2 P4) (P1 P5) (H1 H4))
  (29 (H2 H3) (P2 P3) (H1 H5) (P1 P5))
  (30 (P2 P4) (H2 H3) (H1 H5) (P1 P5))
  (31 (H2 H4) (P2 P3) (H1 H5) (P1 P5))
  (32 (H2 H4) (P2 P4) (H1 H5) (P1 P5)))

(defun count-duplicates (lst)
  (mapcar (lambda (x)
            (count x lst
                   :test-not (lambda (-x -y)
                           (set-difference -x -y :test #'equal))))
          lst))


(let* ((tensors
         '((V (h1 p1) (h2 p2))
           (T2 (p3 h3) (p4 h4))
           (T1 (p5 h5))))
       (symmetries (make-symmetries-in-effective-node-list
                    tensors #'make-node-symmetry))
       (contractions
         '(((P2 P5) (H2 H4) (H1 H3) (P1 P3)) ((H2 H5) (P2 P4) (H1 H3) (P1 P3))
           ((H2 H5) (P2 P5) (H1 H3) (P1 P3)) ((P2 P5) (H2 H3) (H1 H4) (P1 P3))
           ((H2 H5) (P2 P3) (P1 P4) (H1 H3)) ((P2 P5) (H2 H4) (P1 P4) (H1 H3))
           ((H2 H5) (P2 P4) (H1 H4) (P1 P3)) ((H2 H5) (P2 P5) (P1 P4) (H1 H3))
           ((H2 H5) (P2 P5) (H1 H4) (P1 P3)) ((P2 P4) (H2 H3) (H1 H5) (P1 P3))
           ((H2 H4) (P2 P3) (P1 P5) (H1 H3)) ((P2 P5) (H2 H3) (H1 H5) (P1 P3))
           ((H2 H5) (P2 P3) (P1 P5) (H1 H3)) ((H2 H4) (P2 P4) (P1 P5) (H1 H3))
           ((H2 H4) (P2 P4) (H1 H5) (P1 P3)) ((P2 P5) (H2 H4) (H1 H5) (P1 P3))
           ((H2 H5) (P2 P4) (P1 P5) (H1 H3)) ((P2 P5) (H2 H3) (H1 H4) (P1 P4))
           ((H2 H5) (P2 P3) (H1 H4) (P1 P4)) ((H2 H5) (P2 P5) (H1 H4) (P1 P4))
           ((H2 H3) (P2 P3) (P1 P5) (H1 H4)) ((H2 H3) (P2 P3) (H1 H5) (P1 P4))
           ((P2 P4) (H2 H3) (P1 P5) (H1 H4)) ((H2 H4) (P2 P3) (H1 H5) (P1 P4))
           ((P2 P5) (H2 H3) (H1 H5) (P1 P4)) ((H2 H5) (P2 P3) (P1 P5) (H1 H4))
           ((P2 P5) (H2 H4) (H1 H5) (P1 P4)) ((H2 H5) (P2 P4) (P1 P5) (H1 H4))
           ((H2 H3) (P2 P3) (H1 H5) (P1 P5)) ((P2 P4) (H2 H3) (H1 H5) (P1 P5))
           ((H2 H4) (P2 P3) (H1 H5) (P1 P5)) ((H2 H4) (P2 P4) (H1 H5) (P1 P5)))))

  ;; there are no duplicates
  (assert (every (lambda (x) (eq x 1)) (count-duplicates contractions)))
  (let ((sym-conts (filter-contractions-by-symmetries symmetries contractions)))
    (assert-equal sym-conts
                  '(#| 32 |# ((H2 H4) (P2 P4) (H1 H5) (P1 P5))
                    #| 31 |# ((H2 H4) (P2 P3) (H1 H5) (P1 P5))
                    #| 28 |# ((H2 H5) (P2 P4) (P1 P5) (H1 H4))
                    #| 24 |# ((H2 H4) (P2 P3) (H1 H5) (P1 P4))
                    #| 23 |# ((P2 P4) (H2 H3) (P1 P5) (H1 H4))
                    #| 17 |# ((H2 H5) (P2 P4) (P1 P5) (H1 H3))
                    #| 16 |# ((P2 P5) (H2 H4) (H1 H5) (P1 P3))
                    #| 15 |# ((H2 H4) (P2 P4) (H1 H5) (P1 P3))
                    #| 14 |# ((H2 H4) (P2 P4) (P1 P5) (H1 H3))
                    #| 12 |# ((P2 P5) (H2 H3) (H1 H5) (P1 P3))
                    #| 8 |# ((H2 H5) (P2 P5) (P1 P4) (H1 H3))
                    #| 5 |# ((H2 H5) (P2 P3) (P1 P4) (H1 H3))
                    #| 4 |# ((P2 P5) (H2 H3) (H1 H4) (P1 P3))
                    #| 3 |# ((H2 H5) (P2 P5) (H1 H3) (P1 P3))
                    #| 2 |# ((H2 H5) (P2 P4) (H1 H3) (P1 P3))
                    #| 1 |# ((P2 P5) (H2 H4) (H1 H3) (P1 P3))))))
;; Case study: Vijab with T1 and T2 coupling to singles excitations:1 ends here

;; [[file:readme.org::*Finding contractions by target properties][Finding contractions by target properties:2]]
(let ((*filter-node-symmetry* nil)
      (orbital-spaces '((H I J K L h1 h2 h3)
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

    ;; with self-contractions
    (let ((*allow-self-contractions* t))

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
     '()))))
;; Finding contractions by target properties:2 ends here

;; [[file:readme.org::*Finding contractions by target properties][Finding contractions by target properties:4]]
(let ((*allow-self-contractions* t)
      (*filter-node-symmetry* nil))
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
     ((CONTRACTION ((I K))) (F (I J)) (T (C K))))))
;; Finding contractions by target properties:4 ends here

;; [[file:readme.org::*Help routines][Help routines:2]]
(assert-equal (space-subseq :orbital-spaces '((H 1 2 3 4) (P a b c) (G g g2))
                            :from-index 2)
              '((H 3 4) (P c) (G)))
;; Help routines:2 ends here

;; [[file:readme.org::*Help routines][Help routines:4]]
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
;; Help routines:4 ends here

;; [[file:readme.org::*Partitions][Partitions:2]]
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
;; Partitions:2 ends here

;; [[file:readme.org::*Particle hole picture][Particle hole picture:6]]
(in-package :hp)
(import 'arponen::assert-equal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (reset-spaces)
  (print *default-orbital-spaces*)
  (dotimes (i 10)
    (when (zerop (mod i 2)) (genindex 'P))
    (genindex 'H))
  (assert (eq (genindex 'H) 'H11))
  (assert (eq (genindex 'H) 'H12))
  (assert (eq (genindex 'P) 'P6))
  (assert (eq (genindex 'G) 'G1))
  (assert (eq (genindex 'ph) 'ph1))
  (reset-spaces)
  (assert (eq (genindex 'H) 'H1))
  (reset-spaces))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(assert-equal (do-partition-node-description '(H P)
                :partition '((PH H P)))
              '((H P)))
(assert-equal (do-partition-node-description '(H P)
                :partition '((P |a| A)))
              '((H |a|) (H A)))
(assert-equal (do-partition-node-description '(PH P)
                :partition '((PH H P)))
              '((H P) (P P)))
(assert-equal (do-partition-node-description '(PH PH)
                :partition '((PH H P)))
              '((H H) (H P) (P H) (P P)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(reset-spaces)
(assert-equal (name-legs-by-space-name-1 '(t2 (P H) (P H)))
              '(t2 (p1 h1) (p2 h2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(assert-equal (partition-tensor-description '(T2 (HP HP))
                                            :partition '((HP P H)))
              '((T2 (P P)) (T2 (P H)) (T2 (H P)) (T2 (H H))))
(assert-equal (partition-tensor-description '(T2 (HP H))
                                            :partition '((HP P H)))
              '((T2 (P H)) (T2 (H H))))
(assert-equal (partition-tensor-description '(T2 (HP HP) (HP HP))
                                            :partition '((HP H P)))
              '((T2 (H H) (H H))
                (T2 (H H) (H P))
                (T2 (H H) (P H))
                (T2 (H H) (P P))
                (T2 (H P) (H H))
                (T2 (H P) (H P))
                (T2 (H P) (P H))
                (T2 (H P) (P P))
                (T2 (P H) (H H))
                (T2 (P H) (H P))
                (T2 (P H) (P H))
                (T2 (P H) (P P))
                (T2 (P P) (H H))
                (T2 (P P) (H P))
                (T2 (P P) (P H))
                (T2 (P P) (P P))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(filter-tensors-by-symmetries '((((P . H) (H . P)))
                                (((H . P) (P . H))))
                              '((V (P H) (H P))
                                (V (H P) (P H))))

(assert-equal (arponen::apply-symmetries-to-nodes '(((H1 . H3) (H2 . H4))
                                   ((H1 . H2))
                                   ((H3 . H4)))
                                               '(V (H1 H2) (H3 H4)))
              '((V (H3 H4) (H1 H2)) (V (H2 H1) (H3 H4)) (V (H1 H2) (H4 H3))))

(arponen::make-node-symmetry '((P H) (H P)))
(arponen::make-node-symmetry '((P H) (P P) (A B)))
(arponen::apply-symmetry-to-nodes '((P . P) (H . P))
                               '(V (P H) (P P)))


(arponen::apply-symmetry-to-nodes '((P . H) (H . P))
                               '(V (P H) (H P)))
'((V (H1 H2) (H3 H4))      ;; Vijkl
  (V (H13 P5) (H14 H15))   ;; Vijak
  (V (H16 P6) (H17 P7))    ;; Vijab
  (V (P13 H21) (H22 H23))  ;; Vaijk
  (V (P14 H24) (H25 P15))  ;; Vaijb
  (V (P16 H26) (P17 H27))  ;; Vabij
  (V (P21 P22) (H29 H30))  ;; Vaibj
  (V (P23 P24) (H31 P25))  ;; Vaibc
  (V (P26 P27) (P28 H32))  ;; Vabci
  (V (P29 P30) (P31 P32))) ;; Vabcd

(progn (reset-spaces)
       (partition-symmetrize-and-filter '(V (PH PH) (PH PH)) :unrestricted t))

(assert-equal (! V (a b) (c d))
              '(V (a b) (c d)))

(progn
  (reset-spaces)
  (let* ((arponen::*print-log* nil)

         (t2 (!! t2 (P H) (P H)))

         (vaibj (!! V (P P) (H H)))
         (vaijb (!! V (P H) (H P)))
         (V (.+ vaijb vaibj))
         (H (.+ v))
         ;; t1 amplitudes
         (t1 (!! T1 (P H)))
         (fai (!! f (P H)))
         ;; the whole expression to contract
         ;; i.e. Hamiltonian times T amplitudes
         (expression (.* H t1))
         (expression-2 (.* (.+ fai)
                           (.+ 1 t1)))

         ;; targets for the diagrams search space
         (%singles (! _ (P H)))
         (%doubles (! _ (P H) (P H)))

         (contractions (contract %singles expression
                                 :unrestricted t
                                 :only-connected t)))

    (format nil "Processing singles~%")

    (assert-equal (arponen::expr-to-lists expression)
                  '(((V (P3 H3) (H4 P4)) (T1 (P2 H2)))
                    ((V (P5 P6) (H5 H6)) (T1 (P2 H2)))))


    ;; it should be possible to just find an uncontracted diagram
    (assert-equal (contract %singles expression-2
                            :unrestricted t
                            :only-connected t)
                  '(((CONTRACTIONS (NIL)) ((F (P1 H1))))))

    (assert-equal (arponen/hole-particle-picture::remove-1-in-product-list
                   (arponen::expr-to-lists expression-2))
                  '(((F (P1 H1))) ((F (P1 H1)) (T1 (P2 H2)))))

    (assert-equal (contract %doubles (.* vaibj t2)
                            :unrestricted t
                            :only-connected t)
                  '(((CONTRACTIONS (((H5 H7) (P6 P7))))
                     ((V (P5 P6) (H5 H6)) (T2 (P7 H7) (P8 H8))))))

    (let ((c (contract %doubles (.* H t2)
                       :unrestricted t
                       :only-connected t)))
      (assert-equal (length c) 2))
    ))
;; Particle hole picture:6 ends here

;; [[file:readme.org::*TeX][TeX:2]]
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
;; TeX:2 ends here
