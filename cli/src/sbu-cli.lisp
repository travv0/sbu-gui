(defpackage :sbu/cli
  (:use #:cl
        #:serapeum
        #:metabang-bind
        #:alexandria)
  (:export #:main))

(in-package :sbu/cli)

(defparameter *program-name* "sbu")

(defparameter *commands* (dict)
  "Hash table of sub-commands added with the `define-command' macro.")
(defmacro define-command ((command-name function &optional description) &body opts)
  "Define a sub-command with its own command line arguments.

This macro takes a `command-name' as a string, a `function' that should take 2
arguments - the options and free arguments returned by `opts:get-opts' - and an optional
`description' of the command to be shown on the help screen.  The body of this macro
should be plists that `opts:define-opts' would accept, or strings naming the
free arguments this command accepts."
  (bind (((:values free-args opts-descriptions) (partition #'stringp opts)))
    `(setf (@ *commands* ,command-name)
           (list :function ,function
                 :description ,description
                 :free-args ,(string-join free-args " ")
                 :make-opts (lambda ()
                              (opts:define-opts
                                ,@opts-descriptions
                                (:name :help
                                 :description "Print this help"
                                 :short #\h
                                 :long "help")))))))

(define-command ("backup" 'backup "Back up your games.")
  "GAMES..."
  (:name :loop
   :description "Keep running, backing up games at the interval specified in your config file"
   :short #\l
   :long "loop"))

(define-command ("add" 'add "Add a new game to back up.")
  "GAME"
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
   :meta-var "GAME_SAVE_FILE_GLOB"))

(define-command ("list" 'list-games "List games that can be backed up."))

(define-command ("info" 'info "Show detailed info for games that can be backed up.")
  "GAMES...")

(define-command ("remove" 'remove-games "Remove games.")
  "GAMES..."
  (:name :yes
   :description "Remove all without confirmation prompts"
   :short #\y
   :long "yes"))

(define-command ("edit" 'edit "Edit game info.")
  "GAME"
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
   :meta-var "NEW_SAVE_FILE_GLOB"))

(define-command ("config" 'config "Edit program configuration.")
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
   :meta-var "BACKUPS_TO_KEEP"))

(define-condition general-args-error (opts::troublesome-option)
  ((error-string :initarg :error-string :reader error-string))
  (:report (lambda (condition stream)
             (format stream (error-string condition)))))

(define-condition missing-free-args (opts::troublesome-option)
  ((args :initarg :args :accessor args))
  (:report (lambda (condition stream)
             (format stream "missing arguments: ~{~s~^, ~}" (args condition)))))

(define-condition extra-free-args (opts::troublesome-option)
  ((args :initarg :args :accessor args))
  (:report (lambda (condition stream)
             (format stream "extra arguments provided: ~{~s~^, ~}" (args condition)))))

(defun describe-commands (&key prefix suffix usage-of)
  "Print the help screen showing which commands are available.

`prefix' will be printed before the list of available commands.
`suffix' will be printed after the list of available commands.
`usage-of' take the name of the application and uses it to show
how the commands are used."
  (opts:describe :usage-of usage-of
                 :args "COMMAND"
                 :prefix prefix
                 :suffix (~>> *commands*
                              hash-table-alist
                              (sort _ #'string-lessp :key #'car)
                              (mapcar (op (list (car _1) (getf (cdr _1) :description))))
                              (format nil "Available commands:~%~:{~2t~16a~a~%~}~%~@[~a~]"
                                      _ suffix))))

(defun set-opts (command)
  "Set the accepted command line arguments to those relevant to `command'.
Returns T if command exists, NIL otherwise."
  (when-let ((opts-function (getf (@ *commands* command) :make-opts)))
    (funcall opts-function)
    t))

(defun get-command-function (command)
  (getf (@ *commands* command) :function))

(defun get-command-free-args (command)
  (getf (@ *commands* command) :free-args))

(defun help-flag-p (args)
  (or (position "-h" args :test 'string=)
      (position "--help" args :test 'string=)))

(defun backup-file-callback (from to)
  (format t "~%~a ==>~%~4t~a" from to))

(defparameter *day-names*
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter *month-names*
  '("Jan" "Feb" "Mar" "Apr" "May"
    "Jun" "Jul" "Aug" "Sep" "Oct"
    "Nov" "Dec"))

(defun backup-game-callback (game-name finish-time seconds-passed)
  (bind (((:values second minute hour date month year day-of-week _ tz)
          (decode-universal-time finish-time)))
    (format t "~%Finished backing up ~a in ~fs ~
on ~a, ~a ~d ~d at ~2,'0d:~2,'0d:~2,'0d (GMT~@d)~%~%"
            game-name
            seconds-passed
            (nth day-of-week *day-names*)
            (nth (1- month) *month-names*)
            date
            year
            hour
            minute
            second
            (- tz))))

(defun clean-up-callback (files)
  (format t "~%Deleted old backup~p:~:*~[~; ~:;~%~]~{~a~%~}"
          (length files)
          files))

(defun print-warning (restart-function)
  (lambda (condition)
    (format t "Warning: ~a~%" condition)
    (funcall restart-function condition)))

(defun handle-command (command args &optional application-name)
  (if (set-opts command)
      (if (help-flag-p args)
          (opts:describe :usage-of (when application-name
                                     (format nil "~a ~a" application-name command))
                         :args (get-command-free-args command))
          (handler-case
              (bind ((command-function (get-command-function command))
                     ((:values options free-args) (opts:get-opts args)))
                (funcall command-function options free-args))
            (opts::troublesome-option (condition)
              (opts:describe :usage-of (when application-name
                                         (format nil "~a ~a" application-name command))
                             :args (get-command-free-args command)
                             :prefix (format nil "Error: ~a" condition)))))
      (describe-commands :usage-of application-name)))

(defvar *application-catch-errors* nil)

(defun main (&rest args)
  (flet ((error-and-abort (condition debugger-hook)
           (declare (ignore condition debugger-hook))
           (format *error-output* "An unknown error occurred.~%")
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
                   (commands (hash-table-keys *commands*))
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
                          (handler-bind
                              ((sbu:backup-file-error (print-warning #'sbu:skip-file))
                               (sbu:backup-game-error (print-warning #'sbu:skip-game))
                               (sbu:clean-up-error (print-warning #'sbu:skip-clean-up)))
                            (let ((sbu:*backup-file-callback* 'backup-file-callback)
                                  (sbu:*backup-game-callback* 'backup-game-callback)
                                  (sbu:*clean-up-callback* 'clean-up-callback))
                              (handle-command subcommand opts *program-name*))))))
                  (describe-commands :usage-of *program-name*)))))
      (opts::troublesome-option (condition)
        (describe-commands :usage-of *program-name* :prefix condition))
      (sbu:sbu-error (condition)
        (format *error-output* "An error has occurred: ~a~%" condition))
      (file-error (condition)
        (format *error-output* "An error has occurred with file ~a~%"
                (file-error-pathname condition))))))

(defun backup (options free-args)
  (let ((games (sbu:load-games)))
    (loop
      (if (null free-args)
          (sbu:backup-all games)
          (~>> games
               hash-table-alist
               (remove-if-not (op (position (car _)
                                            free-args
                                            :test 'equal)))
               (mapcar #'sbu:backup-game)))
      (unless (getf options :loop)
        (return))
      (sleep (* 60 sbu:*backup-frequency*)))))

(defun add (options free-args)
  (let* ((games (sbu:load-games))
         (game-names (hash-table-keys games))
         (game-name (first free-args)))
    (cond ((null free-args)
           (error 'missing-free-args :args '("GAME")))
          ((> (length free-args) 1)
           (error 'extra-free-args :args (rest free-args)))
          ((position game-name game-names :test #'string=)
           (error "Can't add ~a: already exists." game-name))
          (t (sbu:save-game games
                            game-name
                            (getf options :path)
                            (or (getf options :glob) ""))
             (format t "Added ~a.~%~%" game-name)))))

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
    (cond ((and (null game-names) (null free-args))
           (error 'missing-free-args :args '("GAMES...")))
          ((and (null game-names) free-args)
           (error "Games don't exist: ~{~a~^, ~}" free-args))
          ((or (getf options :yes)
               (progn (format t "Are you sure you'd like to remove the following games from ~a?
~{~a~^, ~}~%"
                              *program-name* free-args)
                      (y-or-n-p "(y/n):")))
           (dolist (game free-args)
             (sbu:remove-game games game))
           (format t "Removed the following games: ~{~a~^, ~}~%~%" free-args))
          (t (format t "No games removed.~%~%")))))

(defun edit (options free-args)
  (let* ((games (sbu:load-games))
         (game-names (hash-table-keys games))
         (old-game-name (first free-args))
         (old-game (@ games old-game-name))
         (old-game-path (getf old-game :save-path))
         (old-game-glob (getf old-game :save-glob))
         (new-game-name (or (getf options :name) old-game-name))
         (new-game-path (getf options :path))
         (new-game-glob (getf options :glob)))
    (cond ((not old-game-name) (error 'missing-free-args :args '("GAME")))
          ((not old-game) (error "~a doesn't exist.  Use `add' command to add it."
                                 old-game-name))
          ((> (length free-args) 1)
           (error 'extra-free-args :args (rest free-args)))
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
             (format t "Name: ~a~@[ -> ~a~]
Save-Path: ~a~@[ -> ~a~]
Save-Glob: ~a~@[ -> ~a~]~%"
                     old-game-name (getf options :name)
                     old-game-path new-game-path
                     old-game-glob new-game-glob)))))

(defun config (options free-args)
  (let* ((config (sbu:load-config))
         (old-backup-path (or (@ config :backup-path)
                              sbu:*backup-path*))
         (old-backup-frequency (or (@ config :backup-frequency)
                                   sbu:*backup-frequency*))
         (old-backups-to-keep (or (@ config :backups-to-keep)
                                  sbu:*backups-to-keep*))
         (new-backup-path (getf options :path))
         (new-backup-frequency (getf options :frequency))
         (new-backups-to-keep (getf options :keep)))
    (cond ((> (length free-args) 0) (error 'extra-free-args :args free-args))
          (t
           (when (or new-backup-path new-backup-frequency new-backups-to-keep)
             (sbu:save-config (dict :backup-path (or new-backup-path old-backup-path)
                                    :backup-frequency (or new-backup-frequency old-backup-frequency)
                                    :backups-to-keep (or new-backups-to-keep old-backups-to-keep))))
           (format t "Backup-Path: ~a~@[ -> ~a~]
Backup-Frequency: ~a~@[ -> ~a~]
Backups-To-Keep: ~a~@[ -> ~a~]~%~%"
                   old-backup-path new-backup-path
                   old-backup-frequency new-backup-frequency
                   old-backups-to-keep new-backups-to-keep)))))
