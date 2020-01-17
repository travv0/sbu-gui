;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

;;; Load the application:
(load "~/code/common-lisp/sbu/build.lisp")

(deliver 'sbu/cli:main
         "~/bin/sbu"
         5
         :keep-pretty-printer t
         :keep-lisp-reader t)
