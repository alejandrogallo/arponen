(defun contraction? (expr)
  (and (listp expr)
       (listp (car expr))
       (eq (caar expr) 'contraction)
       ;; body
       (listp (cadr expr))))

(defun tensor? (expr)
  (and (listp expr)
       (listp (cadr expr))))

(defvar *print-log* t
  "Wether to print the log messages for the contractions and so on")

;; TODO: implement log levels
(defmacro logger (fmt &rest args)
  `(when *print-log*
    (eval (format t ,fmt ,@args))))

(defvar *allow-self-contractions* nil
  "Wether or not to allow a tensor to search for contractions with its
  nodes.")

(defvar *only-connected-diagrams* nil
  "Wether to look for contractions that create connected diagrams.")

(defvar *filter-node-symmetry* t)

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

(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst)))
                              (rest remain))))))

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

(defmacro ordered-subsets-with-repetition (n space-size)
  (let* ((vars (loop for i below (1+ n) collect (gensym))))
    `(let ((,(car vars) 0))
       ,(reduce (lambda (x other-loop)
                  `(loop for ,(cdr x) from ,(car x) below ,space-size
                         ,@(if (null other-loop)
                               `(collect `(,,@(cdr vars)))
                               (list 'nconcing other-loop))))
                (mapcar #'cons vars (cdr vars))
                :initial-value nil
                :from-end t))))

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

(defun symbols-repeated-p (lst)
  (let ((symbols (flatten-list lst))
        s)
    (loop while (setq s (pop symbols))
          if (> (count s symbols) 0)
            do (return t))))

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

(defun match-index-to-space (index orbital-space)
  (find index (cdr orbital-space)))

(defun find-space-by-leg (index orbital-spaces)
  (find index orbital-spaces :test #'match-index-to-space))

(defun find-space-by-name (name orbital-spaces)
  (find name orbital-spaces :key #'car))

(defun find-space-name-by-leg (leg orbital-spaces)
  (car (find leg orbital-spaces :test #'match-index-to-space)))

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

(defun make-node-symmetry (nodes)
  (let* ((n (length nodes))
         (node-combinations
           (loop for fst below n
                 append (loop for snd from (1+ fst) below n
                              collect (list fst snd)))))
    (mapcar (lambda (combi)
              (apply #'mapcar `(cons ,@(mapcar (lambda (i) (nth i nodes))
                                                 combi))))
            node-combinations)))

(defun find-effective-nodes-list (list-of-tensors)
  (let* ((keys (remove-duplicates (mapcar #'car list-of-tensors) :test #'equal))
         (result-alist (mapcar (lambda (k) (cons k '())) keys)))
    (mapc (lambda (tsr)
            (let ((current (assoc (car tsr) result-alist)))
              (rplacd (assoc (car tsr) result-alist)
                    (append (cdr current) (cdr tsr)))))
          list-of-tensors)
    (mapcar #'cdr result-alist)))

(defun make-symmetries-in-list (list-of-tensors)
  (labels ((reducer (x) (reduce #'union x :from-end t)))
    (thread-last list-of-tensors
               (find-effective-nodes-list)
               (mapcar #'make-node-symmetry)
               (reducer))))

(defun find-duplicate-set (element lst)
  (find element lst :test-not (lambda (-x -y)
                                (set-difference -x -y :test #'equal))))

(defun filter-contractions-by-symmetries (symmetries contractions)
  (let ((-contractions (copy-tree contractions)))
    (do (result seen-contractions)
        ((null -contractions) result)
      (let ((c (pop -contractions)))
        (block :sym-searching
          ;; go through all symmetries
          (loop for sym in (cons nil symmetries)
                do (let ((new-c (apply-symmetry-to-nodes sym c)))
                     (when (find-duplicate-set new-c seen-contractions)
                       (push new-c seen-contractions)
                       (logger "~&~a is the same as ~a by virtue of ~a"
                               c new-c sym)
                       (return-from :sym-searching))))
          ;; if I got here, then c is a new contraction
          ;; never seen before
          (push c result)
          (push c seen-contractions))))))

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

(defun get-contracted-temp-tensor (contraction-tensor &key (name 'contracted))
  (let* ((killed-pair '(x x))
         (x-nodes (get-contracted-nodes contraction-tensor
                                        :killed-pair killed-pair))
         (flat-nodes (reduce (lambda (x y) (concatenate 'list x y))
                             x-nodes))
         (cleaned-nodes (remove-if (lambda (x) (equal x killed-pair))
                                   flat-nodes)))
    `(,name ,@cleaned-nodes)))

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
          (labels
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
                       (unless (is-connected-contraction node-pair-combination node-pairs
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
    (let ((cleaned-results (remove-if #'null results)))
      (if *filter-node-symmetry*
          (let ((node-symmetries (make-symmetries-in-list tensor-list)))
            (filter-contractions-by-symmetries node-symmetries cleaned-results))
          cleaned-results))
    ))

(defun find-contractions-in-product-by-target
    (target tensor-list &key
                          orbital-spaces
                          contraction-rules)
  (let ((result (find-contractions-in-product-by-number-of-legs
                 target tensor-list :orbital-spaces orbital-spaces
                                    :contraction-rules contraction-rules)))
    (logger "~&CONTRACTIONS TO CHECK: ~a" result)
    (remove-if #'null
     (loop for contraction in result
          collect
          (let* ((contraction-tensor `((contraction ,contraction)
                                       ,@(copy-list tensor-list)))
                 (contracted-tensor (get-contracted-temp-tensor
                                     contraction-tensor)))

            (logger "~&getting-temp-tensor... ~a ~a" contraction tensor-list)

            (if (match-target-with-tensor target
                                          contracted-tensor
                                          :orbital-spaces orbital-spaces)
                contraction
                nil))))))

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
