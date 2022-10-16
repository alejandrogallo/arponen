;; Parsing test suite
;; :PROPERTIES:
;; :header-args:lisp: :tangle t.lisp :comments both
;; :END:


;; [[file:README.org::*Parsing test suite][Parsing test suite:1]]
(ql:quickload :fiveam)

(in-package :hirata)

(5am:def-suite hirata-parsing)
(defmacro test-parsing (name &rest files)
  `(5am:test (,name :suite hirata-parsing)
     ,(format nil "Test parsing the ~s equations" name)
     ,@(mapcar (lambda (f)
                 `(5am:is (listp (parse-outfile ,f))
                          "Failed parsing file ~s" ,f))
               files)))

(test-parsing ccsdtq
              #P"ccsdtq/ccsdtq_e.out"
              #P"ccsdtq/ccsdtq_t1.out"
              #P"ccsdtq/ccsdtq_t2.out"
              #P"ccsdtq/ccsdtq_t3.out"
              #P"ccsdtq/ccsdtq_t4.out")

(test-parsing ccsdt
              #P"ccsdt/ccsdt_e.out"
              #P"ccsdt/ccsdt_t1.out"
              #P"ccsdt/ccsdt_t2.out"
              #P"ccsdt/ccsdt_t3.out")

(test-parsing ccsd
              #P"ccsd/ccsd_e.out"
              #P"ccsd/ccsd_t1.out"
              #P"ccsd/ccsd_t2.out")

(test-parsing eomccsd
              #P"eomccsd/eomccsd_denominator.out"
              ;; #P"eomccsd/eomccsd_density1.out"
              #P"eomccsd/eomccsd_x1.out"
              #P"eomccsd/eomccsd_x2.out"
              #P"eomccsd/eomccsd_y1.out"
              #P"eomccsd/eomccsd_y2.out")

(test-parsing eomccsdt
              #P"eomccsdt/eomccsdt_denominator.out"
              #P"eomccsdt/eomccsdt_density1.out"
              #P"eomccsdt/eomccsdt_x1.out"
              #P"eomccsdt/eomccsdt_x2.out"
              #P"eomccsdt/eomccsdt_x3.out"
              #P"eomccsdt/eomccsdt_y1.out"
              #P"eomccsdt/eomccsdt_y2.out"
              #P"eomccsdt/eomccsdt_y3.out")

(test-parsing cisd
              #P"cisd/cisd_c1.out"
              #P"cisd/cisd_c2.out"
              #P"cisd/cisd_e.out")

(test-parsing cisdt
              #P"cisdt/cisdt_c1.out"
              #P"cisdt/cisdt_c2.out"
              #P"cisdt/cisdt_c3.out"
              #P"cisdt/cisdt_e.out")

(test-parsing cisdtq
              #P"cisdtq/cisdtq_c1.out"
              #P"cisdtq/cisdtq_c2.out"
              #P"cisdtq/cisdtq_c3.out"
              #P"cisdtq/cisdtq_c4.out"
              #P"cisdtq/cisdtq_e.out")

(fiveam:run! 'hirata-parsing)
;; Parsing test suite:1 ends here
