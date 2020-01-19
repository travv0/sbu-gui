;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

;;; Load the application:
(load "~/code/common-lisp/sbu/gui/build.lisp")

(deliver 'sbu/gui:start
         "~/bin/sbu-gui"
         5
         :interface :capi
         :startup-bitmap-file nil
         :keep-conditions :all
         :keep-pretty-printer t
         :keep-lisp-reader t)
