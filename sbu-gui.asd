(asdf:defsystem #:sbu-gui
  :description "Describe tenhou-dl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "./src/"
  :depends-on (:travv0.prelude :cl-fad :cl-ppcre)
  :components ((:file "sbu-gui")))
