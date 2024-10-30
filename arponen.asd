;; [[file:readme.org::*Prolog][Prolog:1]]
(asdf:defsystem "arponen"
  :description "An many-body equation generator"
  :version "0.0.1"
  :author "Alejandro Gallo <aamsgallo@gmail.com>"
  :licence "GPLv3"
  :depends-on (#:herodot
               #:serapeum
               #:alexandria
               #:rutils)
  :serial t
  :components ((:file "arponen")))


(asdf:defsystem "arponen-test"
  :description "An many-body equation generator"
  :version "0.0.1"
  :author "Alejandro Gallo <aamsgallo@gmail.com>"
  :licence "GPLv3"
  :depends-on (#:arponen
               #:fiveam)
  :serial t
  :components ((:file "t")))
;; Prolog:1 ends here
