;; [[file:../readme.org::*Main routines][Main routines:1]]
(defpackage :herodot
  (:use :cl)
  (:export #:render-sdot
           #:render-sdot!
           #:save))
(in-package :herodot)

(declaim (optimize (debug 3) (safety 3)))

(defun render-sdot (graph &optional stream)
  (eval `(render-sdot! ,graph ,stream)))

(defparameter *default-indentation* "  ")
(defun indent (text &key (indentation *default-indentation*))
  (declare (string text) (string indentation))
  (with-output-to-string (s)
    #1=(princ indentation s)
    (mapc (lambda (c) (princ c s) (when (char= c #\newline) #1#))
          (coerce text 'list))))

(defun indent-list (textlist &key (indentation *default-indentation*))
  (indent (format nil "~{~a~^~%~}" textlist) :indentation indentation))

(defmacro render-sdot! (graph &optional stream)
  `(labels ((:-- (from to &rest opts)
              (format ,stream "~a -- ~a ~a" from to (:-[options] opts)))
            (:-> (from to &rest opts)
              (apply #':-- (append (list from to) (cons '(dir "forward") opts))))
            (:<- (from to &rest opts)
              (apply #':-- (append (list from to) (cons '(dir "back") opts))))
            (:-render-atom (p) (if (symbolp p)
                                  (substitute #\_ #\- (string-downcase
                                                       (symbol-name p)))
                                  p))
            (:-[options] (opts) (format ,stream "[~{~a~^, ~}]" (:-options opts)))
            (:-options (opts) (mapcar (lambda (o)
                                       (etypecase o
                                         (atom (format ,stream "~a" o))
                                         (list (:= (car o) (cadr o)))))
                                     opts))
            (:scope (name &rest els)
              (format ,stream "~&{ /* begin: ~a */~%~a~%} /* end: ~a */"
                      name (indent-list els) name))
            (:= (key var)
              (format ,stream "~a = ~s" (:-render-atom key) (:-render-atom var)))
            (:el (name &rest opts)
              (format ,stream "~(~a~) ~a" name (:-[options] opts)))
            (:node (&rest opts) (apply #':el (cons "node" opts)))
            (:edge (&rest opts) (apply #':el (cons "edge" opts)))
            (:graph-env (&rest opts) (apply #':el (cons "graph" opts)))
            (:cluster (name &rest els) (format ,stream
                                              "subgraph cluster_~a ~a"
                                              name
                                              (apply #':scope (cons "" els))))
            (:list (&rest els) (format nil "~&~{~a~%~}"
                                       (remove-if-not
                                        (lambda (x) (and (atom x)
                                                         (not (functionp x))
                                                         (not (null x))))
                                        els)))
            (:-named-scope (scope-name name els)
              (format ,stream "~a ~a {~%~a~%}"
                      scope-name name
                      (indent (apply #':list els))))
            (:graph (name &rest els) (:-named-scope "graph" name els))
            (:digraph (name &rest els) (:-named-scope "digraph" name els)))
     ,graph))

(defun save (file-basename graph &key (format :svg))
  (let ((dot-string (render-sdot nil graph))
        (out-file (format nil "~a.~(~a~)" file-basename format))
        (dot-file (format nil "~a.dot" file-basename))
        (dot-cmd (format nil "dot -T~(~a~)" format)))
    ;; save dot file
    (with-open-file (f dot-file :if-exists :supersede :direction :output)
      (princ dot-string f))
    ;; create export
    (with-input-from-string (i dot-string)
      (with-open-file (f out-file :if-exists :supersede :direction :output)
      (princ (with-output-to-string (o)
               (uiop:run-program dot-cmd :output o :input i))
             f)))
    out-file))
;; Main routines:1 ends here
