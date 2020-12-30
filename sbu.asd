(asdf:defsystem #:sbu
  :version "0.0.1"
  :serial t
  :pathname "./src/"
  :depends-on (:travv0.utils
               :serapeum
               :metabang-bind
               :cl-fad
               :defstar)
  :components ((:file "sbu"))
  :in-order-to ((test-op (test-op :sbu/tests))))

(asdf:defsystem #:sbu/tests
  :version "0.0.1"
  :serial t
  :pathname "./t/"
  :depends-on (:travv0.utils
               :sb-cover
               :fiveam
               :mockingbird
               :serapeum
               :metabang-bind
               :cl-fad
               :defstar)
  :components ((:file "test-sbu")))

(asdf:defsystem #:sbu/gui
  :version "0.0.1"
  :serial t
  :pathname "./gui/src/"
  :depends-on (:alexandria
               :serapeum
               :metabang-bind
               :cl-fad
               :sbu)
  :components ((:file "sbu-gui")))

(asdf:defsystem #:sbu/cli
  :version "1.0.4"
  :serial t
  :pathname "./cli/src/"
  :depends-on (:sbu
               :unix-opts
               :serapeum
               :travv0.utils
               :metabang-bind
               :alexandria)
  :components ((:file "opt-commands")
               (:file "sbu-cli"))
  :in-order-to ((test-op (test-op :sbu/cli/tests))))

(asdf:defsystem #:sbu/cli/tests
  :serial t
  :pathname "./cli/t/"
  :depends-on (:sbu/cli
               :sbu/tests
               :serapeum
               :travv0.utils
               :cl-fad)
  :components ((:file "test-sbu-cli"))
  :in-order-to ((test-op (test-op :sbu/tests)))
  :perform (asdf:test-op :after (o s)
                         (uiop:symbol-call :sbu/cli/tests '#:run-tests)))
