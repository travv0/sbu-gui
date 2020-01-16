(defpackage :sbu/cli
  (:use #:cl #:serapeum #:metabang-bind #:alexandria))

(in-package :sbu/cli)

(defun set-opts (subcommand)
  (string-case subcommand
    ("backup" (opts:define-opts
                (:name :loop
                 :description "Keep running, backing up games at the interval specified in your config file"
                 :short #\l
                 :long "loop")))

    ("add" (opts:define-opts
             (:name :name
              :description "Game name"
              :short #\n
              :long "name"
              :arg-parser #'identity
              :required t
              :meta-var "GAME_NAME")
             (:name :path
              :description "Game save path"
              :short #\p
              :long "path"
              :arg-parser #'identity
              :required t
              :meta-var "GAME_SAVE_PATH")
             (:name :glob
              :description "Game save file glob"
              :short #\g
              :long "glob"
              :arg-parser #'identity
              :meta-var "GAME_SAVE_FILE_GLOB")))

    ("info" (opts:define-opts))

    ("remove" (opts:define-opts
                (:name :yes
                 :description "Remove all without confirmation prompts"
                 :short #\y
                 :long "yes")))

    ("edit" (opts:define-opts
              (:name :name
               :description "New name"
               :short #\n
               :long "name"
               :arg-parser #'identity
               :meta-var "NEW_NAME")
              (:name :path
               :description "New save path"
               :short #\p
               :long "path"
               :arg-parser #'identity
               :meta-var "NEW_SAVE_PATH")
              (:name :glob
               :description "New save file glob"
               :short #\g
               :long "glob"
               :arg-parser #'identity
               :meta-var "NEW_SAVE_FILE_GLOB")))

    ("config" (opts:define-opts
                (:name :path
                 :description "Path to directory in which to back up saves"
                 :short #\p
                 :long "path"
                 :arg-parser #'identity
                 :meta-var "BACKUP_PATH")
                (:name :frequency
                 :description "Frequency in minutes to backup saves when looping"
                 :short #\f
                 :long "frequency"
                 :arg-parser #'parse-integer
                 :meta-var "BACKUP_FREQUENCY")
                (:name :keep
                 :description "How many copies of each backed-up file to keep"
                 :short #\k
                 :long "keep"
                 :arg-parser #'parse-integer
                 :meta-var "BACKUPS_TO_KEEP")))

    (otherwise (return-from set-opts)))
  t)

(defun main (&optional args)
  (bind (((_ subcommand . opts) (or (and args (cons nil args))
                                    (tu:get-command-line-args))))
    (when (set-opts subcommand)
      (multiple-value-bind (options free-args) (opts:get-opts opts)
        (funcall (read-from-string subcommand)
                 options
                 free-args)))))

(defun backup (options free-args)
  (let ((games (sbu:load-games)))
    (loop
      (if (null free-args)
          (sbu:backup-all games)
          (~>> games
               hash-table-alist
               (remove-if-not (op (position (car _) free-args :test 'equal)))
               (mapcar #'sbu:backup-game)))
      (unless (getf options :loop)
        (return)))))
