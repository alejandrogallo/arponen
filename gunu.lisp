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
(defun get-node-pairs (n &key (group-lengths nil)
  (loop for i from 0 below n
        nconcing (loop for j from i below n
                       collect `(,i ,j))))

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
                    (logger "~&current: ~s matching: ~s through: ~s"
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

(defun get-contracted-temp-tensor (contraction-tensor)
  (let* ((killed-pair '(x x))
         (x-indices (get-contracted-indices contraction-tensor
                                           :killed-pair killed-pair))
         (flat-indices (reduce (lambda (x y) (concatenate 'list x y))
                               x-indices))
         (cleaned-indices (remove-if (lambda (x) (equal x killed-pair))
                                     flat-indices)))
    `(contracted ,@cleaned-indices)))

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
                         (logger "~&~8tcontraction ~a <> ~a through ~a"
                                 a b rule)
                         (list a b))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun symbols-repeated-p (lst)
  (let ((symbols (flatten-list lst))
        s)
    (loop while (setq s (pop symbols))
          if (> (count s symbols) 0)
            do (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-contractions-in-product-by-number-of-legs
    (target tensor-list &key
                          orbital-spaces
                          contraction-rules)
  (let* ((N-c (/ (- (length (flatten-list (mapcar #'cdr tensor-list)))
                    (length (flatten-list (cdr target))))
                 2))
         (all-indices (loop for ts in (mapcar #'cdr tensor-list)
                            with ls = nil
                            do (setq ls (append ls ts))
                            finally (return ls)))
         (space-size (length all-indices))
         ;; '((1 1) (1 2) (2 2)) if length all-indices = 2
         (leg-pairs (get-node-pairs space-size))
         (which-pairs (eval `(ordered-subsets-with-repetition ,N-c
                                                              ,(length leg-pairs))))
         results)
    (logger "~&============")
    (logger "~&N-contractions: ~s" N-c)
    (logger "~&all indices: ~s" all-indices)
    (logger "~&all leg-pairs: ~s" leg-pairs)
    (logger "~&all combinations (of pairs) : ~s" which-pairs)
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
                     (logger "~&combination: ~s pairs: ~s [~s]"
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

(defun latex (tensor-expression &optional (stream t))
  (case (car tensor-expression)
    ('+ (format stream "~&( ~{~a~^~%+ ~}~%)" (mapcar #'latex
                                                     (cdr tensor-expression))))
    ('* (format nil "~{~a ~}" (mapcar #'latex (cdr tensor-expression))))
    (t (latex-tensor tensor-expression))))
