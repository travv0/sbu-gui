(defpackage :sbu/cli
  (:use #:cl
        #:serapeum
        #:metabang-bind
        #:alexandria
        #:opt-commands)
  (:local-nicknames (#:tu #:travv0.utils))
  (:export #:main))

(in-package :sbu/cli)

(defparameter *program-name* "sbu")

(define-command ("backup" 'backup "Back up your games.")
  :free-args (("GAMES" :count :many))
  :options ((:name :loop
             :description "Keep running, backing up games at the interval specified in your config file"
             :short #\l
             :long "loop")
            (:name :verbose
             :description "Print verbose output"
             :short #\v
             :long "verbose")))

(define-command ("add" 'add "Add a new game to back up.")
  :free-args (("GAME" :required t))
  :options ((:name :path
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

(define-command ("list" 'list-games "List games that can be backed up."))

(define-command ("info" 'info "Show detailed info for games that can be backed up.")
  :free-args (("GAMES" :count :many)))

(define-command ("remove" 'remove-games "Remove games.")
  :free-args (("GAMES" :count :many :required t))
  :options ((:name :yes
             :description "Remove all without confirmation prompts"
             :short #\y
             :long "yes")))

(define-command ("edit" 'edit "Edit game info.")
  :free-args (("GAME" :required t))
  :options ((:name :name
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

(define-command ("config" 'config "Edit program configuration.")
  :options ((:name :path
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

(defun backup-file-callback (from to)
  (format t "~%~a ==>~%~4t~a" from to)
  (force-output))

(defparameter *day-names*
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter *month-names*
  '("Jan" "Feb" "Mar" "Apr" "May"
    "Jun" "Jul" "Aug" "Sep" "Oct"
    "Nov" "Dec"))

(defun clean-up-callback (files)
  (when *verbose*
    (format *error-output* "~%Deleted old backup~p:~:*~[~; ~:;~%~]~{~a~%~}"
            (length files)
            files)))

(defparameter *verbose* nil
  "Whether to print verbose output.")

(defvar *application-catch-errors* nil)

(defun main (&rest args)
  (flet ((error-and-abort (condition debugger-hook)
           (declare (ignore debugger-hook))
           (format *error-output* "Error: ~a~%" condition)
           (return-from main)))
    (handler-case
        (handler-bind ((opts:unknown-option (lambda (c)
                                              (declare (ignore c))
                                              (invoke-restart 'opts:skip-option))))
          (let ((*debugger-hook* (if *application-catch-errors*
                                     #'error-and-abort
                                     *debugger-hook*))
                (*program-name* (file-namestring (or (first (uiop:raw-command-line-arguments))
                                                     *program-name*))))
            (opts:define-opts
              (:name :games-path
               :description "Path to games configuration file."
               :short #\g
               :long "games-path"
               :arg-parser #'identity
               :meta-var "GAMES_CONFIG_PATH")
              (:name :config-path
               :description (string+ "Path to " *program-name* " configuration file.")
               :short #\c
               :long "config-path"
               :arg-parser #'identity
               :meta-var "PROGRAM_CONFIG_PATH"))

            (let* ((full-args (or (and args (cons nil args))
                                  (uiop:raw-command-line-arguments)))
                   (commands (commands))
                   (command-position (position-if (op (position _ commands :test #'string=))
                                                  full-args)))
              (if command-position
                  (let* ((pre-command-args (take command-position full-args))
                         (options (opts:get-opts pre-command-args))
                         (games-path (getf options :games-path))
                         (config-path (getf options :config-path))
                         (args (drop command-position full-args))
                         (sbu:*games-path* (or games-path sbu:*games-path*))
                         (sbu:*config-path* (or config-path sbu:*config-path*))
                         (config (sbu:load-config))
                         (sbu:*backup-frequency* (or (@ config :backup-frequency)
                                                     sbu:*backup-frequency*))
                         (sbu:*backup-path* (or (@ config :backup-path)
                                                sbu:*backup-path*))
                         (sbu:*backups-to-keep* (or (@ config :backups-to-keep)
                                                    sbu:*backups-to-keep*)))
                    (if (null args)
                        (describe-commands :usage-of *program-name*)
                        (bind (((subcommand . opts) args))
                          (handle-command subcommand opts *program-name*))))
                  (describe-commands :usage-of *program-name*)))))
      (opts:troublesome-option (condition)
        (describe-commands :usage-of *program-name* :prefix condition)))))

(defun backup (options free-args)
  (let (warnings current-warnings)
    (flet ((backup-game-callback (game-name file-count finish-time seconds-passed)
             (when (plusp file-count)
               (bind (((:values second minute hour date month year day-of-week _ tz)
                       (decode-universal-time finish-time)))
                 (format *error-output* "~%Finished backing up ~d file~:p~@[ with ~d warning~:p~] for ~a in ~fs ~
on ~a, ~a ~d ~d at ~2,'0d:~2,'0d:~2,'0d (GMT~@d)~%~%"
                         file-count
                         (unless (null current-warnings) (length current-warnings))
                         game-name
                         seconds-passed
                         (nth day-of-week *day-names*)
                         (nth (1- month) *month-names*)
                         date
                         year
                         hour
                         minute
                         second
                         (- tz))
                 (appendf warnings (reverse current-warnings))
                 (setf current-warnings nil))))
           (print-warning (restart-function)
             (lambda (condition)
               (push condition current-warnings)
               (format *error-output* "~%Warning: ~a~%" condition)
               (funcall restart-function condition))))
      (handler-bind
          ((sbu:backup-file-error (print-warning #'sbu:skip-file))
           (sbu:backup-game-error (lambda (c)
                                    (funcall
                                     (if (find-restart 'sbu:skip-file)
                                         (funcall (print-warning #'sbu:skip-file)
                                                  (sbu:inner-error c))
                                         (funcall (print-warning #'sbu:skip-game) c)))))
           (sbu:clean-up-error (print-warning #'sbu:skip-clean-up)))
        (let ((games (sbu:load-games))
              (*verbose* (getf options :verbose))
              (sbu:*backup-game-callback* #'backup-game-callback)
              (sbu:*backup-file-callback* #'backup-file-callback)
              (sbu:*clean-up-callback* #'clean-up-callback))
          (loop
            (unwind-protect
                 (if (null free-args)
                     (sbu:backup-all games)
                     (~>> games
                          hash-table-alist
                          (remove-if-not (op (position (car _)
                                                       free-args
                                                       :test 'equal)))
                          (mapcar #'sbu:backup-game)))
              (when (plusp (length warnings))
                (format *error-output* "~d warning~:p occurred:~%"
                        (length warnings))
                (if *verbose*
                    (format *error-output* "~%~{~a~%~%~}" warnings)
                    (format *error-output* "Pass --verbose flag to print all warnings after backup completes~%~%"))))
            (unless (getf options :loop)
              (return))
            (sleep (* 60 sbu:*backup-frequency*))))))))

(defun add (options free-args)
  (let* ((games (sbu:load-games))
         (game-names (hash-table-keys games))
         (game-name (first free-args)))
    (cond ((position game-name game-names :test #'string=)
           (error "Can't add ~a: already exists." game-name))
          (t (sbu:save-game games
                            game-name
                            (tu:canonicalize-path (getf options :path))
                            (or (getf options :glob) ""))
             (format *error-output* "Added ~a.~%~%" game-name)))))

(defun list-games (options free-args)
  (declare (ignore options free-args))
  (let ((games (sbu:load-games)))
    (format t "~{~a~%~}~%"
            (sort (hash-table-keys games) #'string-lessp))))

(defun info (options free-args)
  (declare (ignore options))
  (let* ((games (sbu:load-games))
         (games-alist (sort (hash-table-alist games) #'string-lessp :key #'car)))
    (format t "~{~{Name: ~a~%~@{~:(~a~): ~a~%~}~}~%~}"
            (if (null free-args)
                games-alist
                (remove-if-not (op (position (car _)
                                             free-args
                                             :test 'equal))
                               games-alist)))))

(defun remove-games (options free-args)
  (let* ((games (sbu:load-games))
         (existing-game-names (hash-table-keys games))
         (game-names (remove-if-not (op (position _ existing-game-names
                                                  :test #'string=))
                                    free-args)))
    (cond ((and (null game-names) free-args)
           (error "Games don't exist: ~{~a~^, ~}" free-args))
          ((or (getf options :yes)
               (y-or-n-p "Are you sure you'd like to remove the following games from ~a?
~{~a~^, ~}~%"
                         *program-name* free-args))
           (dolist (game free-args)
             (sbu:remove-game games game))
           (format *error-output* "Removed the following games: ~{~a~^, ~}~%~%" free-args))
          (t (format *error-output* "No games removed.~%~%")))))

(defun edit (options free-args)
  (let* ((games (sbu:load-games))
         (game-names (hash-table-keys games))
         (old-game-name (first free-args))
         (old-game (@ games old-game-name))
         (old-game-path (getf old-game :save-path))
         (old-game-glob (getf old-game :save-glob))
         (new-game-name (or (getf options :name) old-game-name))
         (new-game-path (tu:canonicalize-path (getf options :path)))
         (new-game-glob (getf options :glob)))
    (cond ((not old-game) (error "~a doesn't exist.  Use `add' command to add it."
                                 old-game-name))
          ((and (string/= old-game-name new-game-name)
                (position new-game-name game-names :test #'string=))
           (error "Can't rename to ~a: already exists." new-game-name))
          ((not (or (getf options :name) new-game-path new-game-glob))
           (error 'general-args-error
                  :error-string "at least one of \"--name\", \"--path\", or \"--glob\" must be provided"))
          (t (sbu:save-game games
                            new-game-name
                            (or new-game-path old-game-path)
                            (or new-game-glob old-game-glob "")
                            old-game-name)
             (format *error-output* "Name: ~a~@[ -> ~a~]
Save-Path: ~a~@[ -> ~a~]
Save-Glob: ~a~@[ -> ~a~]~%"
                     old-game-name (getf options :name)
                     old-game-path new-game-path
                     old-game-glob new-game-glob)))))

(defun config (options free-args)
  (declare (ignore free-args))
  (let* ((config (sbu:load-config))
         (old-backup-path (or (@ config :backup-path)
                              sbu:*backup-path*))
         (old-backup-frequency (or (@ config :backup-frequency)
                                   sbu:*backup-frequency*))
         (old-backups-to-keep (or (@ config :backups-to-keep)
                                  sbu:*backups-to-keep*))
         (new-backup-path (tu:canonicalize-path (getf options :path)))
         (new-backup-frequency (getf options :frequency))
         (new-backups-to-keep (getf options :keep)))
    (when (or new-backup-path new-backup-frequency new-backups-to-keep)
      (sbu:save-config (dict :backup-path (or new-backup-path old-backup-path)
                             :backup-frequency (or new-backup-frequency old-backup-frequency)
                             :backups-to-keep (or new-backups-to-keep old-backups-to-keep))))
    (format *error-output* "Backup-Path: ~a~@[ -> ~a~]
Backup-Frequency: ~a~@[ -> ~a~]
Backups-To-Keep: ~a~@[ -> ~a~]~%~%"
            old-backup-path new-backup-path
            old-backup-frequency new-backup-frequency
            old-backups-to-keep new-backups-to-keep)))
