;;;; zonehopper.asd

(asdf:defsystem #:zonehopper
  :serial t
  :description "Describe zonehopper here"
  :author "Futuro"
  :license "GPLv3"
  :depends-on (#:postmodern)
  :components ((:file "package")
               (:file "zonehopper")))

