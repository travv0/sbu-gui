(asdf:defsystem #:sbu
    :version "0.0.1"
    :serial t
    :pathname "./src/"
    :depends-on (:travv0.utils
                 :serapeum
                 :metabang-bind
                 :cl-fad)
    :components ((:file "sbu")))

(asdf:defsystem #:sbu/gui
    :version "0.0.1"
    :serial t
    :pathname "./gui/src/"
    :depends-on (:alexandria
                 :serapeum
                 :metabang-bind
                 :sbu)
    :components ((:file "sbu-gui")))
