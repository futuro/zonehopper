;;;; zonehopper.asd

(asdf:defsystem #:zonehopper
  :serial t
  :description "Experimental Mass Transit Passenger route finding algorithm."
  :author "Futuro"
  :license "GPLv3"
  :depends-on (#:postmodern)
  :components ((:file "package")
               (:file "zonehopper")))

