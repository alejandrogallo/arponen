;; [[file:readme.org::*herodot][herodot:1]]
(asdf:defsystem "herodot"
  :description "Another s-expression language for dot"
  :version "0.0.1"
  :author "Alejandro Gallo <aamsgallo@gmail.com>"
  :licence "GPLv3"
  :depends-on (#:uiop)
  :serial t
  :components ((:file "herodot/herodot")
               (:file "herodot/goldstone")))
;; herodot:1 ends here
