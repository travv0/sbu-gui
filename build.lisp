(lw:set-default-character-element-type 'character)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(push #P"~/code/" ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :sbu/cli)
