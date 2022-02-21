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

(declaim (ftype (function (integer)) get-node-pairs))
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

(defun flatten-list (ls)
  (cond
    ((and (consp ls)
          (atom (car ls)))
     `(,(car ls) ,@(flatten-list (cdr ls))))
    ((and (consp ls)
          (consp (car ls)))
     `(,@(flatten-list (car ls)) ,@(flatten-list (cdr ls))))
    (t ls)))

(defun symbols-repeated-p (lst)
  (let ((symbols (flatten-list lst))
        s)
    (loop while (setq s (pop symbols))
          if (> (count s symbols) 0)
            do (return t))))

(defun expression-to-lists (exp)
  (ecase (car exp)
    ('* (let ((operands
                (mapcar (lambda (e) (case (car e)
                                      ('+ (cdr e))
                                      (t (list e))))
                        (cdr exp))))
          operands))))

(defun expand-expression (expr)
  (eval `(cartesian-product ,@(expression-to-lists expr))))

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
          do
             (case (length (intersection node contraction))
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
                                          (subst stiched node result)))))
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

;;todo this is not good enough
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
          do (let ((db (make-hash-table))
                   path
                   node last-node)
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

(defun find-contractions-in-product-by-number-of-legs
    (target tensor-list &key
                          orbital-spaces
                          contraction-rules)
  (let* ((N-c (/ (- (length (flatten-list (mapcar #'cdr tensor-list)))
                    (length (flatten-list (cdr target))))
                 2))
         (all-nodes (reduce #'append (mapcar #'cdr tensor-list)))
         (space-size (length all-nodes))
         (group-lengths (mapcar #'length (cdr tensor-list)))
         ;; '((1 1) (1 2) (2 2)) if length all-nodes = 2
         (node-pairs (get-node-pairs space-size
                                     :group-lengths
                                     (unless *allow-self-contractions*
                                       group-lengths)))
         (which-pairs (eval
                       `(ordered-subsets-with-repetition ,N-c
                                                         ,(length node-pairs))))
         results)
    (logger "~&============")
    (logger "~&N-contractions: ~s" N-c)
    (logger "~&all nodes: ~s" all-nodes)
    (logger "~&all node-pairs: ~s" node-pairs)
    (logger "~&all combinations (of pairs) : ~s" which-pairs)
    (setq results
          (labels
              ((indexing (indices lst) (mapcar (lambda (i) (nth i lst))
                                               indices)))
            (loop
              for pair-indices in which-pairs
              nconcing
              (block :pairs-discovery
                (tagbody
                   (let* ((pairs (indexing pair-indices node-pairs))
                          (nodes (mapcar (lambda (x)
                                           (indexing x all-nodes)) pairs))
                         top-contractions)
                     (logger "~&combination: ~s pairs: ~s [~s]"
                             pair-indices
                             pairs nodes)
                     ;; todo
                     (when *only-connected-diagrams*
                       nil)
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
  (let ((products (expand-expression expression))
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
    ('+ (format stream "~&( ~{~a~^~%+ ~}~%)" (mapcar #'latex
                                                     (cdr tensor-expression))))
    ('* (format nil "~{~a ~}" (mapcar #'latex (cdr tensor-expression))))
    (t (latex-tensor tensor-expression))))
