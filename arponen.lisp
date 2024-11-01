;; [[file:readme.org::*Prolog][Prolog:2]]
(defpackage :arponen
  (:use :cl))
(in-package :arponen)
;; Prolog:2 ends here

;; [[file:readme.org::*Types][Types:3]]
(defun contraction? (expr)
  (and (listp expr)
       (listp (car expr))
       (eq (caar expr) 'contraction)
       ;; body
       (listp (cadr expr))))

(defun tensor? (expr)
  (and (listp expr)
       (listp (cadr expr))))
;; Types:3 ends here

;; [[file:readme.org::*Verbosity and logging][Verbosity and logging:1]]
(defvar *print-log* t
  "Wether to print the log messages for the contractions and so on")

;; TODO: implement log levels
(defmacro logger (fmt &rest args)
  `(when *print-log*
    (eval (format t ,fmt ,@args))))

(defmacro logger! (&rest args)
  `(let ((*print-log* t))
    (logger ,@args)))
;; Verbosity and logging:1 ends here

;; [[file:readme.org::*Contractions within the same tensor][Contractions within the same tensor:1]]
(defvar *allow-self-contractions* nil
  "Wether or not to allow a tensor to search for contractions with its
  nodes.")
;; Contractions within the same tensor:1 ends here

;; [[file:readme.org::*Connected diagrams][Connected diagrams:1]]
(defvar *only-connected-diagrams* nil
  "Wether to look for contractions that create connected diagrams.")
;; Connected diagrams:1 ends here

;; [[file:readme.org::*Node symmetry][Node symmetry:1]]
(defvar *filter-node-symmetry* t)
;; Node symmetry:1 ends here

;; [[file:readme.org::*Parity symmetry][Parity symmetry:1]]
(defvar *filter-parity-symmetry* nil
  "Wether to filter contractions according to parity symmetry.")
;; Parity symmetry:1 ends here

;; [[file:readme.org::*Cartesian product][Cartesian product:1]]
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
;; Cartesian product:1 ends here

;; [[file:readme.org::*Permutations][Permutations:1]]
(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst)))
                              (rest remain))))))
;; Permutations:1 ends here

;; [[file:readme.org::*Node pairs building][Node pairs building:1]]
(defun get-node-pairs (n &key (group-lengths nil))
  ;; check that group-lengths is well built
  (when group-lengths (assert (eq n (apply #'+ group-lengths))))
  (let ((successive-lengths
          ;; successive-lengths
          ;; should be simply (g0 (+ g0 g1) ... (+ g0 .. gn))
          ;; where gj \in group-lengths
          (reverse (maplist (lambda (lst) (apply #'+ lst))
                            (reverse group-lengths)))))
    (labels ((from-i (i)
             (if group-lengths
                 ;; find the first group where i
                 ;; is smaller, this means the next group
                 ;; starts there
                 (find i successive-lengths :test #'<)
                 i)))
    (loop for i from 0 below n
        nconcing (loop for j from (from-i i) below n
                       collect `(,i ,j))))))
;; Node pairs building:1 ends here

;; [[file:readme.org::*Pair combinations][Pair combinations:2]]
(defmacro ordered-subsets-with-repetition (n space-size)
  (when (> n 0)
    (let* ((vars (loop for i below (1+ n) collect (gensym))))
      `(let ((,(car vars) 0))
         ,(reduce (lambda (x other-loop)
                    `(loop for ,(cdr x) from ,(car x) below ,space-size
                           ,@(if (null other-loop)
                                 `(collect `(,,@(cdr vars)))
                                 (list 'nconcing other-loop))))
                  (mapcar #'cons vars (cdr vars))
                  :initial-value nil
                  :from-end t)))))
;; Pair combinations:2 ends here

;; [[file:readme.org::*Utils][Utils:1]]
;; functions taken from uruk
(defun flatten-list (ls)
  (cond
    ((and (consp ls)
          (atom (car ls)))
     `(,(car ls) ,@(flatten-list (cdr ls))))
    ((and (consp ls)
          (consp (car ls)))
     `(,@(flatten-list (car ls)) ,@(flatten-list (cdr ls))))
    (t ls)))

(defmacro thread-first (var &rest forms)
  (let ((init var))
    (loop for f in forms
          do (setf init (setf f (cons (car f)
                                      (cons init (cdr f))))))
    init))

(defmacro thread-last (var &rest forms)
  (let ((init var))
    (loop for f in forms
          do (setf init (setf f (cons (car f)
                                      (reverse (cons init
                                                     (reverse (cdr f))))))))
    init))
;; Utils:1 ends here

;; [[file:readme.org::*Utils][Utils:3]]
(defun symbols-repeated-p (lst)
  (let ((symbols (flatten-list lst))
        s)
    (loop while (setq s (pop symbols))
          if (> (count s symbols) 0)
            do (return t))))
;; Utils:3 ends here

;; [[file:readme.org::*Arithmetic expressions][Arithmetic expressions:1]]
(defun expr-to-lists (exp)
    (case (if (atom exp) t (car exp))
      (* (reduce (lambda (x y)
                   (reduce #'append
                           (loop for -x in x
                                 collect (loop for -y in y
                                               collect (append -x -y)))))
                 (mapcar #'expr-to-lists (cdr exp))
                 :initial-value '(nil)
                 :from-end t))
      (+ (reduce #'append (mapcar #'expr-to-lists (cdr exp))))
      (t (list (list exp)))))

(defun expr-power (n expr)
  `(* ,@(mapcar (constantly expr) (loop for i below n collect nil))))
;; Arithmetic expressions:1 ends here

;; [[file:readme.org::*Index spaces][Index spaces:1]]
(defun match-index-to-space (index orbital-space)
  (find index (cdr orbital-space)))
;; Index spaces:1 ends here

;; [[file:readme.org::*Index spaces][Index spaces:3]]
(defun find-space-by-leg (index orbital-spaces)
  (find index orbital-spaces :test #'match-index-to-space))
;; Index spaces:3 ends here

;; [[file:readme.org::*Index spaces][Index spaces:5]]
(defun find-space-by-name (name orbital-spaces)
  (find name orbital-spaces :key #'car))

(defun find-space-name-by-leg (leg orbital-spaces)
  (car (find leg orbital-spaces :test #'match-index-to-space)))
;; Index spaces:5 ends here

;; [[file:readme.org::*Index spaces][Index spaces:8]]
(defun traverse-nodes (fn tensor)
  (destructuring-bind (name . nodes) tensor
    `(,name ,@(mapcar fn nodes))))

(defun traverse-legs (fn tensor)
  (traverse-nodes (lambda (node) (mapcar fn node)) tensor))

(defun tensor-to-description (tensor &key orbital-spaces)
  (traverse-legs (lambda (leg) (find-space-name-by-leg leg orbital-spaces))
                 tensor))
;; Index spaces:8 ends here

;; [[file:readme.org::*Tensor sum][Tensor sum:1]]
(defun tensor-sum (&rest expressions)
  `(+ ,@(reduce (lambda (tsr rest)
                  (if (atom tsr)
                      (cons tsr rest)
                      (case (car tsr)
                        (+ (append (cdr tsr) rest))
                        (t (cons tsr rest)))))
                expressions
                :from-end t
                :initial-value nil)))
;; Tensor sum:1 ends here

;; [[file:readme.org::*Tensor matching][Tensor matching:1]]
(defun match-target-with-tensor-1 (target tensor &key orbital-spaces)
  (unless (eq (length target) (length tensor))
    (return-from match-target-with-tensor-1 nil))
  (notany #'null
          (loop for target-tensor in (mapcar #'list (cdr target) (cdr tensor))
                collect
                (let ((spaces (mapcar (lambda (i) (find i orbital-spaces :key #'car))
                                      (car target-tensor))))
                  (assert (eq (length (car target-tensor)) (length (cadr target-tensor))))
                  (notany #'null (mapcar #'match-index-to-space
                                         (cadr target-tensor)
                                         spaces))))))
;; Tensor matching:1 ends here

;; [[file:readme.org::*Tensor matching][Tensor matching:3]]
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
;; Tensor matching:3 ends here

;; [[file:readme.org::*Node symmetry][Node symmetry:2]]
(defun apply-symmetry-to-nodes (symmetry-equivalence object)
  (let* ((temp-symbols (mapcar (lambda (x) (declare (ignorable x))
                                 (gensym)) symmetry-equivalence))
         (equiv-forward (mapcar (lambda (x y) (cons (cdr x) y))
                                symmetry-equivalence temp-symbols))
         (equiv-backward (mapcar (lambda (x y) (cons y (car x)))
                                 symmetry-equivalence temp-symbols)))
    (sublis equiv-backward
            (sublis symmetry-equivalence
                    (sublis equiv-forward object)))))


(defun apply-symmetries-to-nodes (symmetry-equivalences object)
  (mapcar (lambda (x) (apply-symmetry-to-nodes x object)) symmetry-equivalences))
;; Node symmetry:2 ends here

;; [[file:readme.org::*Node symmetry][Node symmetry:4]]
(defun triangle-pairs (n)
  (loop for fst below n
        append (loop for snd from (1+ fst) below n
                     collect (list fst snd))))

(defun iota (n &key (start 0))
  (declare (optimize (safety 0) (speed 3) (debug 0)) (fixnum n) (fixnum start))
  (loop for i below n collect (+ i start)))

(defun all-transpositions (ls &key (test #'eq) )
  (loop for perm in (all-permutations ls)
        collect (remove-duplicates (loop for i in perm
                                         for j in ls
                                         if (not (funcall test i j))
                                           collect (cons i j))
                                   :test
                                   (lambda (x y)
                                     (and (eq (car x) (cdr y))
                                          (eq (cdr x) (car y)))))))

(defun make-node-symmetry (nodes)
  (flet ((idxs-to-syms (idxs)
           (loop for idx in idxs
                 append (let ((node-a (nth (car idx) nodes))
                              (node-b (nth (cdr idx) nodes)))
                          (loop for a in node-a
                                for b in node-b
                                collect (cons a b))))))
    (let* ((combinations (all-transpositions (iota (length nodes)))))
      (remove-if #'null (mapcar #'idxs-to-syms combinations)))))
;; Node symmetry:4 ends here

;; [[file:readme.org::*Node symmetry][Node symmetry:6]]
(defun find-effective-nodes-list (list-of-tensors)
  (let* ((keys (remove-duplicates (mapcar #'car list-of-tensors) :test #'equal))
         (result-alist (mapcar (lambda (k) (cons k '())) keys)))
    (mapc (lambda (tsr)
            (let ((current (assoc (car tsr) result-alist)))
              (rplacd (assoc (car tsr) result-alist)
                    (append (cdr current) (cdr tsr)))))
          list-of-tensors)
    (mapcar #'cdr result-alist)))

(defun make-symmetries-in-node-list (list-of-tensors sym-maker)
  (labels ((reducer (x) (reduce #'union x :from-end t)))
    (thread-last list-of-tensors
                 (mapcar sym-maker)
                 (reducer))))

(defun make-symmetries-in-effective-node-list (list-of-tensors sym-maker)
  (make-symmetries-in-node-list (find-effective-nodes-list list-of-tensors)
                                sym-maker))
;; Node symmetry:6 ends here

;; [[file:readme.org::*Antisymmetry][Antisymmetry:1]]
(defun unzip (ls)
  (loop for i below (apply #'min (mapcar #'length ls))
        collect (mapcar (lambda (l) (nth i l)) ls)))

(defun make-antisymmetry-symmetry (nodes)
  (assert (every (lambda (n) (eq (length n) (length (car nodes)))) nodes)
          nil
          "Antisymmetry is not expected to work for tensors with~%~4t~a~%~a"
          nodes "an unequal number of legs per node")
  (let* ((legs-list (unzip nodes))
         (nlegs (length (car legs-list)))
         (tpairs (triangle-pairs nlegs))
         (single-symmetries
           (mapcar (lambda (legs)
                     (mapcar (lambda (pair) (cons (nth (car pair) legs)
                                                  (nth (cadr pair) legs)))
                             tpairs))
                   legs-list)))
    ;;
    ;; TODO: think about including also the products or not
    ;;
    ;; (append (mapcar #'list (apply #'append single-symmetries))
    ;;         (eval `(cartesian-product ,@single-symmetries)))
    (mapcar #'list (apply #'append single-symmetries))))

(defun make-parity-symmetry--1 (nodes)
  (let* ((legs-list (unzip nodes))
         (iota (iota (length (car legs-list))))
         (legs-transpositions (mapcar #'all-transpositions legs-list)))
    (assert (eq (length legs-list) 2))
    (mapcar (lambda (x) (reduce #'append x))
            (eval `(cartesian-product ,@legs-transpositions)))))


(defun make-nodes-difference (nodes-a nodes-b)
  (let (result)
    (loop for node-a in nodes-a
          for node-b in nodes-b
          do (loop for a in node-a
                       for b in node-b
                       unless (eq a b)
                         unless (or (assoc b result)
                                    (assoc a result))
                           do (push (cons a b) result)))
    (reverse result)))

#+nil
(assert-equal (make-nodes-difference '((a b) (e f)) '((a c) (e h)))
              '((b . c) (f . h)))

(progn
  ;; todo: improve this, it is too expensive
  (defun make-parity-symmetry (nodes)
    (let* ((node-symmetries (make-node-symmetry nodes))
           ;; apply node symmetries to the nodes
           (new-nodes (cons nodes
                            (mapcar (lambda (sym)
                                      (apply-symmetry-to-nodes sym nodes))
                                    node-symmetries)))
           ;; for every node let us take all the parity symmetries
           (parity-symmetries (mapcar #'make-parity-symmetry--1 new-nodes))
           (nodes-with-parity
             (mapcar (lambda (syms -nnodes)
                       (mapcar (lambda (sym)
                                 (apply-symmetry-to-nodes sym -nnodes))
                               syms))
                     parity-symmetries new-nodes))
           (node-differences (mapcar (lambda (-nodes)
                                       (make-nodes-difference nodes -nodes))
                                     (apply #'concatenate
                                            (cons 'list nodes-with-parity)))))
      (remove-if #'null node-differences)))

  (make-parity-symmetry '((a i) (b j) (c k)))
  (make-parity-symmetry '((a i) (b j))))

(apply 'concatenate '(list (a b c) (d e f)))

;(apply-symmetry-to-nodes '((a . b)) '(T2 a))
(apply-symmetry-to-nodes nil '(T2 a))
(make-parity-symmetry--1 '((a i) (b j)))
(make-parity-symmetry--1 '((a i)))
(make-parity-symmetry--1 '((a i) (b j) (c k)))
;; Antisymmetry:1 ends here

;; [[file:readme.org::*Filtering contractions through symmetries][Filtering contractions through symmetries:1]]
(defun find-duplicate-set (element lst)
  (find element lst :test-not (lambda (-x -y)
                                (set-difference -x -y :test #'equal))))

(defun pair-set-difference (pa pb)
  (check-type pa cons)
  (check-type pb cons)
  (flet ((c-to-list (c) (list (car c) (cdr c))))
    (set-difference (c-to-list pa) (c-to-list pb))))

(defun find-duplicate-pair-set (element lst)
  (find element lst :test-not
        (lambda (x y)
          (set-difference x y
                          :test-not #'pair-set-difference))))
;; Filtering contractions through symmetries:1 ends here

;; [[file:readme.org::*Filtering contractions through symmetries][Filtering contractions through symmetries:3]]
(defun filter-contractions-by-symmetries (symmetries contractions)
  (let ((-contractions (copy-tree contractions)))
    (do (result seen-contractions)
        ((null -contractions) result)
      (let ((c (pop -contractions)))
        (block :sym-searching
          ;; go through all symmetries
          (loop for sym in (cons nil symmetries)
                do (let ((new-c (apply-symmetry-to-nodes sym c)))
                     (when (find-duplicate-pair-set new-c seen-contractions)
                       (push new-c seen-contractions)
                       (logger "~&~a is the same as ~a by virtue of ~a"
                               c new-c sym)
                       (return-from :sym-searching))))
          ;; if I got here, then c is a new contraction
          ;; never seen before
          (push c result)
          (push c seen-contractions))))))
;; Filtering contractions through symmetries:3 ends here

;; [[file:readme.org::*Mergin nodes][Mergin nodes:1]]
(defun stich-together (contraction node-a node-b)
  ;; contraction-assoc: ((c0 . x) (c1 . x))
  (let ((contraction-assoc (mapcar (lambda (x) (cons x 'x)) contraction)))
      (labels ((kill-matching (i) (sublis contraction-assoc i)))
    (let* ((killed-a (kill-matching node-a))
           (pos-a (position 'x killed-a))
           (killed-b (kill-matching node-b))
           (pos-b (position 'x killed-b)))
      (when (or (equal killed-a node-a)
                (equal killed-b node-b))
        (error "The contraction ~a does not link nodes ~a and ~a"
               contraction node-a node-b))
      (if (eq pos-a pos-b) ;; NUCLEAR-TODO
          (error "You are trying to contract ~a and ~a at the same position ~a"
                 node-a node-b pos-a)
          (progn
            (setf (nth pos-a node-a) (car (delete 'x killed-b)))
            node-a))))))
;; Mergin nodes:1 ends here

;; [[file:readme.org::*Mergin nodes][Mergin nodes:3]]
(defun find-and-replace-matching-nodes (contraction tensor-nodes-list
                                        &key killed-pair)
  "tensor-nodes-list is a list of list of nodes"
  (let* ((result (copy-tree tensor-nodes-list))
         (all-nodes-flat (reduce #'append result)))
    (loop for node in all-nodes-flat
          do (case (length (intersection node contraction))
               (0 (continue))
               ;; self-contraction
               (2 (return (subst killed-pair node result :test #'equal)))
               ;; usual contraction
               ;; x--<>---
               ;; we should find exactly ONE OTHER PLACE where this
               ;; contraction is linked by the contraction
               ;; otherwise it is an error
               (1 (let ((matching-nodes
                          (remove-if
                           (lambda (x) (or (equal x node)
                                           (not (intersection x contraction))))
                           all-nodes-flat)))
                    (logger "~&current: ~s matching: ~s through: ~s"
                            node matching-nodes contraction)
                    (case (length matching-nodes)
                      (0 (error "Unbound contractiong ~a with ~a"
                                node contraction))
                      (1 (let ((stiched (stich-together contraction
                                                        node
                                                        (car matching-nodes))))
                           (return (subst killed-pair
                                          (car matching-nodes)
                                          (subst stiched node result
                                                 :test #'equal)
                                          :test #'equal))))
                      (t
                       (error "Contraction arity(~a) error ~a contracts with ~a"
                              (length matching-nodes) node matching-nodes)))
                    ))))))
;; Mergin nodes:3 ends here

;; [[file:readme.org::*Mergin nodes][Mergin nodes:5]]
(defun get-contracted-nodes (contraction-tensor &key killed-pair)
  ;; todo replace with contraction-p
  (assert (eq (caar contraction-tensor) 'contraction))
  (let ((contracted-nodes (copy-list (mapcar #'cdr (cdr contraction-tensor))))
        (contractions (cadar contraction-tensor)))
    (loop for contraction in contractions
          do
             (setq contracted-nodes
                   (find-and-replace-matching-nodes contraction
                                                    contracted-nodes
                                                    :killed-pair killed-pair)))
    contracted-nodes))
;; Mergin nodes:5 ends here

;; [[file:readme.org::*Effective temporary tensor][Effective temporary tensor:1]]
(defun get-contracted-temp-tensor (contraction-tensor &key (name 'contracted))
  (let* ((killed-pair '(x x))
         (x-nodes (get-contracted-nodes contraction-tensor
                                        :killed-pair killed-pair))
         (flat-nodes (reduce (lambda (x y) (concatenate 'list x y))
                             x-nodes))
         (cleaned-nodes (remove-if (lambda (x) (equal x killed-pair))
                                   flat-nodes)))
    `(,name ,@cleaned-nodes)))
;; Effective temporary tensor:1 ends here

;; [[file:readme.org::*Compatible contractions][Compatible contractions:1]]
(defun compatible-contractions (node-a node-b &key
                                                orbital-spaces
                                                contraction-rules)
  (declare (cons node-a) (cons node-b))
  (assert (and (eq (length node-a) 2) (eq (length node-a) (length node-b))))
  (remove-if
   #'null
   (mapcar (lambda (rule)
             (destructuring-bind ((space-a space-b) pos-a pos-b) rule
               (let ((a (nth pos-a node-a))
                     (b (nth pos-b node-b)))
                 (when (and (eq (find-space-name-by-leg a orbital-spaces)
                                space-a)
                            (eq (find-space-name-by-leg b orbital-spaces)
                                space-b))
                   (list a b)))))
           contraction-rules)))
;; Compatible contractions:1 ends here

;; [[file:readme.org::*Checking for connectedness][Checking for connectedness:1]]
(defun is-connected-contraction (pair-combination node-pairs &key group-lengths)
  (let* ((psums (mapcar (lambda (ls) (apply #'+ ls))
                        (maplist #'identity (reverse group-lengths))))
         ;; an interval represents a diagram
         (intervals (mapcar #'cons psums (append (cdr psums) '(0))))
         (diagrams-names (mapcar (lambda (i) (cons i (gensym "DIAGRAM-")))
                                 intervals))
         (node-indices (mapcar (lambda (pair-index) (nth pair-index node-pairs))
                               pair-combination)))
    ;; TODO: optimize this...
    (labels ((diagram-of (i)
               (cdr (assoc (find-if (lambda (interval)
                                      (and (> (car interval) i)
                                           (>= i (cdr interval))))
                                    intervals)
                           diagrams-names))))
      (block :main-routine
        (loop
          for node-permutation in (all-permutations node-indices)
          do (let (path)
               (block :current-permutation
                 (tagbody
                    (loop for node in node-permutation
                          do (let ((diagrams (mapcar #'diagram-of node)))
                               (if (equal (intersection diagrams path)
                                          diagrams)
                                   (return-from :current-permutation)
                                   (progn
                                     (setq path
                                           (append
                                            path
                                            (set-difference diagrams path)))
                                     (when (>= (length path)
                                               (length group-lengths))
                                       (return-from :main-routine t))))))
                    ))
               ))))
    ))
;; Checking for connectedness:1 ends here

;; [[file:readme.org::*Finding contractions by number of legs][Finding contractions by number of legs:1]]
#+nil
(defun is-a-contraction-possible-by-number-of-legs
    (target tensor-list &key
                          orbital-spaces
                          contraction-rules)
  (let* ((N-c (/ (- (length (flatten-list (mapcar #'cdr tensor-list)))
                    (length (flatten-list (cdr target))))
                 2))
         (all-nodes (copy-tree (reduce #'append (mapcar #'cdr tensor-list))))
         (group-lengths (mapcar (lambda (tsr) (length (cdr tsr))) tensor-list))
         ;; '((1 1) (1 2) (2 2)) if length all-nodes = 2
         (node-pairs (get-node-pairs (length all-nodes)
                                     :group-lengths
                                     (unless *allow-self-contractions*
                                       group-lengths)))
         (node-pair-combinations
           (eval `(ordered-subsets-with-repetition ,N-c
                                                   ,(length node-pairs))))
         results)

  ))
;; Finding contractions by number of legs:1 ends here

;; [[file:readme.org::*Finding contractions by number of legs][Finding contractions by number of legs:2]]
(defun find-contractions-in-product-by-number-of-legs
    (target tensor-list &key
                          orbital-spaces
                          contraction-rules)
  (let* ((N-c (/ (- (length (flatten-list (mapcar #'cdr tensor-list)))
                    (length (flatten-list (cdr target))))
                 2))
         (all-nodes (reduce #'append (mapcar #'cdr tensor-list)))
         (group-lengths (mapcar (lambda (tsr) (length (cdr tsr))) tensor-list))
         ;; '((1 1) (1 2) (2 2)) if length all-nodes = 2
         (node-pairs (get-node-pairs (length all-nodes)
                                     :group-lengths
                                     (unless *allow-self-contractions*
                                       group-lengths)))
         (node-pair-combinations
           (eval `(ordered-subsets-with-repetition ,N-c
                                                   ,(length node-pairs))))
         results)
    (logger "~&============")
    (logger "~&N-contractions: ~s" N-c)
    (logger "~&all nodes: ~s" all-nodes)
    (logger "~&all node-pairs: ~s" node-pairs)
    (logger "~&all combinations (of pairs) : ~s" node-pair-combinations)
    (setq results
          (flet
              ((indexing (indices lst) (mapcar (lambda (i) (nth i lst))
                                               indices)))
            (loop
              for node-pair-combination in node-pair-combinations
              nconcing
              (block :pairs-discovery
                (tagbody
                   (let* ((pairs (indexing node-pair-combination node-pairs))
                          (nodes (mapcar (lambda (x)
                                           (indexing x all-nodes)) pairs))
                          (II 0)
                          top-contractions)
                     (logger "~&combination: ~s pairs: ~s [~s]"
                             node-pair-combination
                             pairs nodes)
                     (incf II)
                     (when *only-connected-diagrams*
                       (unless (is-connected-contraction node-pair-combination
                                                         node-pairs
                                                         :group-lengths
                                                         group-lengths)
                         (return-from :pairs-discovery)))
                     (loop for pair in pairs
                           collect
                           (let* ((vertices (indexing pair all-nodes))
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
                                (logger "~&~30t⇐Exiting since ~a fully in ~a"
                                        conts top-contractions)
                                (return-from :pairs-discovery))
                               (t
                                (logger "~&~8tvertices: ~s" vertices)
                                (logger "~&~24t appending contractions ~s" conts)
                                (push conts top-contractions)))))

                     ;; START FILTERING
                     (return-from :pairs-discovery
                       (let (--result)
                         (mapc (lambda (real-contraction)
                                 ;; photons say: repeated letters must go!
                                 (let ((letters (flatten-list real-contraction)))
                                   (unless (symbols-repeated-p letters)
                                     (pushnew real-contraction
                                              --result
                                              :test-not
                                              (lambda (x y) (set-difference
                                                             x y
                                                             :test #'equal))))))
                               (eval `(cartesian-product
                                       ,@top-contractions)))
                         --result))
                     ))))))
    (let ((cleaned-results (remove-if #'null results))
          symmetries)
      (format t "~&~5tCompute symmetries")
      (when *filter-node-symmetry*
        (format t "~&~10tnode symms")
        (setq symmetries
              (append symmetries (make-symmetries-in-effective-node-list
                                  tensor-list #'make-node-symmetry))))
      (when *filter-parity-symmetry*
        (format t "~&~10tparity symms")
        (setq symmetries
              (append symmetries (make-symmetries-in-node-list
                                  (mapcar #'cdr tensor-list)
                                  #'make-parity-symmetry))))
      (format t "~&~5tResults BEFORE cleaning ~a" (length cleaned-results))
      (setq cleaned-results
            (filter-contractions-by-symmetries symmetries
                                               cleaned-results))
      (format t "~&~5tResults AFTER Cleaning ~a" (length cleaned-results))
      cleaned-results)))
;; Finding contractions by number of legs:2 ends here

;; [[file:readme.org::*Finding contractions by target properties][Finding contractions by target properties:1]]
(defun find-contractions-in-product-by-target
    (target tensor-list &key
                          orbital-spaces
                          contraction-rules)
  (let ((result (find-contractions-in-product-by-number-of-legs
                 target tensor-list :orbital-spaces orbital-spaces
                                    :contraction-rules contraction-rules)))
    (logger "~&CONTRACTIONS TO CHECK: ~a" result)
    (remove-if (lambda (x) (eq x :no-match))
     (loop for contraction in (cons nil result)
          collect
          (let* ((contraction-tensor `((contraction ,contraction)
                                       ,@(copy-list tensor-list)))
                 (contracted-tensor (get-contracted-temp-tensor
                                     contraction-tensor)))

            (logger "~&temp-tensor... ~a" contracted-tensor)

            (if (match-target-with-tensor target
                                          contracted-tensor
                                          :orbital-spaces orbital-spaces)
                contraction
                :no-match))))))
;; Finding contractions by target properties:1 ends here

;; [[file:readme.org::*Finding contractions by target properties][Finding contractions by target properties:3]]
(defun contract-expressions-by-target
    (target expression &key orbital-spaces contraction-rules)
  (let ((products (expr-to-lists expression))
        sums)
    (setq sums
          (loop
            for product in products
            appending
            (progn (print product)
                   (let ((contractions
                           (find-contractions-in-product-by-target target product
                                                                   :orbital-spaces
                                                                   orbital-spaces
                                                                   :contraction-rules
                                                                   contraction-rules)))
                     (mapcar (lambda (x) `((contraction ,x) ,@product))
                             contractions)))))
    `(+ ,@sums)))
;; Finding contractions by target properties:3 ends here

;; [[file:readme.org::*Help routines][Help routines:1]]
(defun space-subseq (&key orbital-spaces from-index)
  (mapcar (lambda (space)
            (handler-case `(,(car space)
                            ,@(subseq (cdr space) from-index))
              (condition ()
                (error (concatenate
                        'string
                        "Dear user: "
                        "When partitioning tensors, all spaces "
                        "should have a long enough length to cut "
                        "through the leg names using from-index. "
                        "~&In this case "
                        "the space ~s needs at least more "
                        "than ~s elements "
                        "BUT it currently has ~s ")
                       space from-index (length (cdr space))))))
          orbital-spaces))
;; Help routines:1 ends here

;; [[file:readme.org::*Help routines][Help routines:3]]
(defun name-legs-by-space-name (tensor-description &key orbital-spaces (from-index 0))
  (let ((orbital-spaces-copy (copy-tree
                              (space-subseq :orbital-spaces orbital-spaces
                                            :from-index from-index))))

    `(,(car tensor-description)
      ,@(loop for index-description in (cdr tensor-description)
              collect
              (loop for space-name in index-description
                    collect
                    (let ((space (find-space-by-name space-name orbital-spaces-copy)))
                      (if (cdr space)
                          (pop (cdr space))
                          (error "Not enough leg names given for space ~a~%"
                                 space))))))
    ))
;; Help routines:3 ends here

;; [[file:readme.org::*Partitions][Partitions:1]]
(defun partition-tensor (tensor &key orbital-spaces partition (from-index 0))
  (let ((name (car tensor))
        (indices (cdr tensor))
        (orbital-spaces-copy (copy-tree
                              (space-subseq :orbital-spaces orbital-spaces
                                            :from-index from-index)))
        new-indices-unexpanded)
    (setq
     new-indices-unexpanded
     (mapcar
      (lambda (index)
        (mapcar
         (lambda (leg)
           (let* ((space (find-space-by-leg leg orbital-spaces))
                  (space-name (car space))
                  (partition (find space-name partition :key #'car)))
             (if partition
                 ;; we found a partition
                 (mapcar (lambda (-space-name)
                           (let* ((space (find-space-by-name
                                          -space-name
                                          orbital-spaces-copy)))
                             (if (cdr space) ;; available leg names
                                 (pop (cdr space))
                                 (error "Not enough leg names given for space ~a~%"
                                        space))))
                         ;; elements of the partition (e.g H P)
                         (cdr partition))
                 (list leg))))
         index))
      indices))
    (let ((new-indices (eval `(cartesian-product
                               ,@(mapcar (lambda (index-set)
                                           (eval `(cartesian-product ,@index-set)))
                                         new-indices-unexpanded)))))
      `(+ ,@(mapcar (lambda (ids) `(,name ,@ids))
                   new-indices)))))
;; Partitions:1 ends here

;; [[file:readme.org::*Particle hole picture][Particle hole picture:5]]
(defpackage :arponen/hole-particle-picture
  (:use :cl :arponen)
  (:nicknames :hp))
(in-package :arponen/hole-particle-picture)

(defvar *default-orbital-spaces*
  '((H)   ;; holes
    (P)   ;; particles
    (G)   ;; general (or rather ghosts)
    (PH)) ;; particle-holes (real vacuum)
  "Orbital space for the default particle-hole picture")
(defvar *orbital-spaces* (copy-tree *default-orbital-spaces*))

(defvar *default-orbital-spaces-counter*
  '((H . 0)
    (P . 0)
    (G . 0)
    (PH . 0))
  "Current index for the orbital spaces")
(defvar *orbital-spaces-counter* (copy-tree *default-orbital-spaces-counter*))

(defvar *default-space-partition*
  '((PH H P)))
(defvar *space-partition* (copy-tree *default-space-partition*))

(defvar *contraction-rules* '(((H H) 0 1)
                              ((P P) 1 0))
  "The conctractions that are not zero.")

(defun reset-spaces ()
  (setq *orbital-spaces-counter* (copy-tree *default-orbital-spaces-counter*))
  (setq *orbital-spaces* (copy-tree *default-orbital-spaces*))
  (values *orbital-spaces* *orbital-spaces-counter*))

;; simple useful functions for knowing which kind of index we have
(defun hole-p (idx) (char= (char (symbol-name idx) 0) #\H))
(defun particle-p (idx) (char= (char (symbol-name idx) 0) #\P))

(defun genindex (space-name)
  (let ((counter (find space-name *orbital-spaces-counter* :key #'car))
        (space (position space-name *orbital-spaces* :key #'car)))
    (unless (and counter space) (error "~&The name ~s is not one of ~s"
                                       space-name
                                       (mapcar #'car *orbital-spaces*)))
    (let ((new-index
            (intern (format nil "~a~a" space-name (incf (cdr counter))))))
      (setf (nth space *orbital-spaces*)
            (append (nth space *orbital-spaces*) (list new-index)))
      new-index)))

(defun name-legs-by-space-name-1 (tensor-description)
  (arponen::traverse-legs #'genindex tensor-description))


(defun do-partition-node-description (node &key partition)
  (eval `(arponen::cartesian-product
          ,@(mapcar (lambda (leg)
                      (let ((p (find leg partition :key #'car)))
                        (if p (cdr p) (list leg))))
                    node))))


(defun partition-tensor-description (tensor-description &key partition)
  (destructuring-bind (name . nodes) tensor-description
    (let* ((p-node-lists (mapcar (lambda (n)
                                   (do-partition-node-description n
                                     :partition partition)) nodes))
           (new-node-lists (eval `(arponen::cartesian-product ,@p-node-lists))))
      (mapcar (lambda (nodes) `(,name ,@nodes)) new-node-lists))))


(defun remove-1-in-product-list (prod-list)
  (mapcar (lambda (product)
            (remove-if (lambda (el) (or (eq el 1) (equal el '(1))))
                       product))
          prod-list))

(defun filter-tensors-by-symmetries (symmetries-list tensor-list)
  (let (result)
    (mapc (lambda (sym tsr)
            (let ((new-tsrs (arponen::apply-symmetries-to-nodes sym tsr)))
              (unless (intersection (cons tsr new-tsrs) result :test #'equal)
                (push tsr result))))
          symmetries-list tensor-list)
    (reverse result)))

(defun filter-tensors-by-symmetries-and-description
    (symmetries tensor-list &key orbital-spaces)
  (mapcar #'cadr
          (remove-duplicates
           (mapcar #'list symmetries tensor-list)
           :test
           (lambda (x y)
             (let* ((all-x (cons (cadr x)
                                 (arponen::apply-symmetries-to-nodes (car x) (cadr x))))
                    (all-y (cons (cadr y)
                                 (arponen::apply-symmetries-to-nodes (car y) (cadr y))))
                    (x-descr (mapcar (lambda (-x)
                                       (arponen::tensor-to-description -x
                                                                       :orbital-spaces
                                                                       orbital-spaces))
                                     all-x))
                    (y-descr (mapcar (lambda (-y)
                                       (arponen::tensor-to-description -y
                                                                       :orbital-spaces
                                                                       orbital-spaces))
                                     all-y)))
               (intersection x-descr y-descr :test #'equal))))))

(defun partition-symmetrize-and-filter (tensor-description &key unrestricted)
  (let* ((tensors (mapcar #'name-legs-by-space-name-1
                          (partition-tensor-description tensor-description
                                                        :partition
                                                        *space-partition*)))
         (arponen::*filter-parity-symmetry* unrestricted)
         symmetries)
    (print tensors)
    (when arponen::*filter-node-symmetry*
      (setq symmetries
            (mapcar (lambda (x) (arponen::make-node-symmetry (cdr x)))
                    tensors)))
    (when arponen::*filter-parity-symmetry*
      (setq symmetries
            (append symmetries
                    (mapcar (lambda (x)
                              (arponen::make-parity-symmetry (cdr x)))
                            (remove-duplicates
                             (reduce #'append
                                     (arponen::apply-symmetries-to-nodes symmetries
                                                                         tensors))
                             :test #'equal)))))
    (filter-tensors-by-symmetries-and-description symmetries
                                                  tensors
                                                  :orbital-spaces
                                                  *orbital-spaces*)))
(defmacro ! (name &rest nodes)
  `'(,name ,@nodes))

(defmacro !! (name &rest nodes)
  (let ((tensor-description `(,name ,@nodes)))
    `'(+ ,@(partition-symmetrize-and-filter tensor-description))))

(defmacro .* (&rest args)
  `(list '* ,@args))

(defmacro .+ (&rest args)
  `(arponen::tensor-sum ,@args))

;; TODO: node-symmetry ein und auschalten
(defun contract (target expression &key (node-symmetry t) (only-connected nil)
                                     (unrestricted nil))
  (let* ((expanded (remove-1-in-product-list (arponen::expr-to-lists expression)))
         (n (length expanded))
         (arponen::*only-connected-diagrams* only-connected)
         (arponen::*allow-self-contractions* nil)
         (arponen::*filter-parity-symmetry* unrestricted)
         (i 0))
    (remove-if #'null
               (mapcar (lambda (tensor-product)
                         (format t "~&[~a/~a] ~a" (incf i) n tensor-product)
                         (let ((begin (get-internal-run-time))
                               (contractions
                                 (arponen::find-contractions-in-product-by-target
                                  target
                                  tensor-product
                                  :orbital-spaces
                                  *orbital-spaces*
                                  :contraction-rules
                                  *contraction-rules*)))
                           (format t "~2t in (~,1f seconds)"
                                   (/ (- (get-internal-run-time) begin)
                                      internal-time-units-per-second))
                           (when contractions
                             (list `(contractions ,contractions) tensor-product))))
                       expanded))))

(defun save-contractions (file-name contractions &key format)
  (with-open-file (s file-name :if-exists :supersede :direction :output)
    (case format
      (t (format s "~s" contractions)))))
;; Particle hole picture:5 ends here

;; [[file:readme.org::*TeX][TeX:1]]
(defun latex-tensor (tensor)
  (format nil "~a^{~a}_{~a}"
          (car tensor)
          (format nil "~{~a~}" (mapcar #'car (cdr tensor)))
          (format nil "~{~a~}" (mapcar #'cadr (cdr tensor)))))

(defun latex (tensor-expression &optional (stream nil))
  (case (car tensor-expression)
    (+ (format stream "~&( ~{~a~^~%+ ~}~%)" (mapcar #'latex
                                                     (cdr tensor-expression))))
    (* (format nil "~{~a ~}" (mapcar #'latex (cdr tensor-expression))))
    (t (latex-tensor tensor-expression))))
;; TeX:1 ends here

#|
(save "v@c-pp-hp-t1-t2"
      `(graph "diagram"
              ,(style)
              ,(make-top '(:top1 :top2 :top3 :top4))
              (scope "V"
                    (node '(color blue))
                    ,(tensor 2 "v" "V"))
              ,(tensor 1 "t1" "T")
              ,(tensor 2 "t2" "T")
              (scope "t1 times v"
                     ,(particle "t1:0" "v:0")
                     ,(particle "v:0" :top1)
                     ,(hole "t1:0" :top2))
              (scope "t2 times V"
                     ,(particle "t2:0" "v:1")
                     ,(hole "t2:0" "v:1")
                     ,(particle "t2:1" :top3)
                     ,(hole "t2:1" :top4))))
|#

(defpackage arponen/dot
  (:use :cl :herodot :arponen))
(in-package arponen/dot)

(defun top (i)
  (intern (format nil "TOP~a" i) "KEYWORD"))

(defun make-top (contraction)
  (let ((n (- (length (arponen::flatten-list (mapcar #'cdr (cadr contraction))))
              (length (arponen::flatten-list (cdar contraction))))))
    (loop for i from 1 to n collect (top i))))

(make-top
 '((CONTRACTIONS (((P116 P84) (P114 P83))))
   ((V (P113 P114) (P115 P116))
    (T2 (P83 H83) (P84 H84)))))

(defun render-contraction-pair (cnt tensors)
  (let (c-tensors)
    ;; find tensors participating in the contraction
    (loop :for tsr :in tensors
          :if (intersection cnt (arponen::flatten-list (cdr tsr)))
            :do (push tsr c-tensors))
    (labels ((coords (leg)
               (let* ((tsr-idx (position leg c-tensors
                                         :test
                                         (lambda (i r)
                                           (member i (arponen::flatten-list r)))))
                      (tsr (nth tsr-idx c-tensors))
                      (inode (position leg (cdr tsr)
                                       :test
                                       (lambda (i r)
                                         (member i
                                                 (arponen::flatten-list r))))))
                 (list :itensor tsr-idx
                       :inode inode
                       :label (let ((name (symbol-name (car tsr))))
                                (format nil "~(~a~):~a" name inode))
                       :hole-p (hp::hole-p leg)))))
      (let (left right hole-or-particle (max-itensor -1))
        (loop :for leg :in cnt
              :do (destructuring-bind (&key itensor inode label hole-p) (coords leg)
                    (setq hole-or-particle
                          (if hole-p #'herodot::hole #'herodot::particle))
                    (if (> itensor max-itensor)
                        (setq right left
                              left label
                              max-itensor itensor)
                        (setq right label))))
        (funcall hole-or-particle right left)))))


(render-contraction-pair
 '(P116 P84)
 '((V (P113 P114) (P115 P116))
   (T2 (P83 H83) (P84 H84))))

(defun render-heaven (contraction heaven)
  (let ((contracted-indices (arponen::flatten-list (cdar contraction))))
    (flet ((is-contracted (idx) (member idx contracted-indices)))
      (let (coords)
        (loop :for tsr :in (cadr contraction)
              :for c :from 0
              :collect
              (let ((ileg 0))
                (arponen::traverse-legs
                 (lambda (leg)
                   (unless (is-contracted leg)
                     (push (list :itensor c
                                 :ileg ileg
                                 :name (format nil "~(~a~):~a"
                                               (car tsr)
                                               (position leg (cdr tsr)
                                                  :test (lambda (i x)
                                                          (member i x))))
                                 :top (pop heaven)
                                 :leg leg
                                 :hole-p (hp::hole-p leg))
                           coords))
                   (incf ileg))
                 tsr)))
        (loop :for coord :in (reverse coords)
              :collect
              (destructuring-bind (&key itensor top name ileg inode leg hole-p)
                  coord
                (if hole-p
                    (herodot::hole name top)
                    (herodot::particle name top))))))))

(render-heaven
 #1='((CONTRACTIONS (((P116 P84) (P114 P83))))
        ((V (P113 P114) (P115 P116))
         (T2 (P83 H83) (P84 H84))))
 (make-top #1#))

(defun render (contraction)
  "Render only one diagram, this means,
   do not pass a set of diagrams in the format of
   many contractions like

   ((contractions ((...) (...)))
    (tensors...))"
  (assert (eq (length (cdar contraction)) 1))
  (herodot:render-sdot
   (let ((top (make-top contraction)))
     `(:graph "diagram"
              ,(herodot::style)
              ,(herodot::make-top top)
              ,@(mapcar (lambda (tsr)
                          (let ((lbl (symbol-name (car tsr))))
                            (herodot::tensor (length (cdr tsr))
                                             lbl lbl)))
                        (cadr contraction))
              (:scope "Contractions"
                      ,@(let ((cts (cadar contraction))
                              (tsrs (cadr contraction)))
                          (loop :for ct :in (car cts)
                                :collect (render-contraction-pair ct tsrs))))
              (:scope "Heaven"
                      ,@(render-heaven
                                contraction
                                top))
              ))))

#+(or)
(progn
  #1=(render
      '((CONTRACTIONS (((P116 P84) (P114 P83))))
        ((V (P113 P114) (P115 P116))
         (T2 (P83 H83) (P84 H84)))))
  (format t "~a" #1#))

(defun render-all (contractions)
  "Return a list of strings of dot graphics."
  (destructuring-bind (conts tensors) contractions
    (mapcar #+nil (lambda (cnt) (list :cnt cnt #'identity))
            (lambda (cont)
              (render `((contractions (,cont))
                        ,tensors)))
            (cadar contractions))))

#+(or)
(render-all '((CONTRACTIONS
               (((P91 P82) (H101 H83) (H100 H82) (P90 P83)) ((H101 H82) (P91 P82) (H100 H84) (P90 P83))
                ((P91 P82) (H101 H81) (P90 P84) (H100 H83)) ((P91 P82) (H101 H83) (H100 H84) (P90 P83))
                ((H101 H81) (P91 P83) (P90 P84) (H100 H83)) ((H101 H81) (P91 P81) (H100 H83) (P90 P83))
                ((P91 P81) (H101 H84) (H100 H83) (P90 P83))))
              ((V (H100 P90) (H101 P91)) (T2 (P83 H83) (P84 H84)) (T2 (P81 H81) (P82 H82)))))
