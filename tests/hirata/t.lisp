;; Parsing test suite
;; :PROPERTIES:
;; :header-args:lisp: :tangle t.lisp :comments both
;; :END:


;; [[file:README.org::*Parsing test suite][Parsing test suite:1]]
(ql:quickload :fiveam)
#+slynk
(setq fiveam:*run-test-when-defined* t)

(in-package :hirata)

(fiveam:in-suite* hirata-parsing)
(defmacro test-parsing (name &rest files)
  (let ((lisp-files (mapcar (lambda (f)
                              (let ((ext ".out"))
                                (format nil "~a.lisp"
                                        (subseq f 0 (- (length f)
                                                       (length ext))))))
                            files)))
    `(5am:test (,name :suite hirata-parsing)
     ,(format nil "Test parsing the ~s equations" name)
     ,@(mapcar (lambda (f lf)
                 `(5am:is (equal (parse-outfile ,f)
                                 (uiop:read-file-form ,lf))
                          #+nil
                          "File ~a parsed is not ~a" #+nil ,f #+nil ,lf))
               files
               lisp-files))))

(test-parsing ccsdtq
              "ccsdtq/ccsdtq_e.out"
              "ccsdtq/ccsdtq_t1.out"
              "ccsdtq/ccsdtq_t2.out"
              "ccsdtq/ccsdtq_t3.out"
              "ccsdtq/ccsdtq_t4.out")

(test-parsing ccsdt
              "ccsdt/ccsdt_e.out"
              "ccsdt/ccsdt_t1.out"
              "ccsdt/ccsdt_t2.out"
              "ccsdt/ccsdt_t3.out")

(test-parsing ccsd
              "ccsd/ccsd_e.out"
              "ccsd/ccsd_t1.out"
              "ccsd/ccsd_t2.out")

(test-parsing eomccsd
              "eomccsd/eomccsd_denominator.out"
              "eomccsd/eomccsd_density1.out"
              "eomccsd/eomccsd_x1.out"
              "eomccsd/eomccsd_x2.out"
              "eomccsd/eomccsd_y1.out"
              "eomccsd/eomccsd_y2.out")

(test-parsing eomccsdt
              "eomccsdt/eomccsdt_denominator.out"
              "eomccsdt/eomccsdt_density1.out"
              "eomccsdt/eomccsdt_x1.out"
              "eomccsdt/eomccsdt_x2.out"
              "eomccsdt/eomccsdt_x3.out"
              "eomccsdt/eomccsdt_y1.out"
              "eomccsdt/eomccsdt_y2.out"
              "eomccsdt/eomccsdt_y3.out")

(test-parsing cisd
              "cisd/cisd_c1.out"
              "cisd/cisd_c2.out"
              "cisd/cisd_e.out")

(test-parsing cisdt
              "cisdt/cisdt_c1.out"
              "cisdt/cisdt_c2.out"
              "cisdt/cisdt_c3.out"
              "cisdt/cisdt_e.out")

(test-parsing cisdtq
              "cisdtq/cisdtq_c1.out"
              "cisdtq/cisdtq_c2.out"
              "cisdtq/cisdtq_c3.out"
              "cisdtq/cisdtq_c4.out"
              "cisdtq/cisdtq_e.out")


;; (fiveam:run! 'hirata-parsing)
;; Parsing test suite:1 ends here
