(defpackage :sbu/cli
  (:use #:cl
        #:serapeum
        #:metabang-bind
        #:alexandria
        #:opt-commands)
  (:local-nicknames (#:tu #:travv0.utils))
  (:export #:main #:sbu))

(in-package :sbu/cli)

(defparameter *program-name* "sbu")

(define-command ("backup" 'backup "Back up your games")
  :free-args (("GAMES" :count :many))
  :options ((:name :loop
             :description "Keep running, backing up games at the interval specified in your config file"
             :short #\l
             :long "loop")
            (:name :verbose
             :description "Print verbose output"
             :short #\v
             :long "verbose")))

(define-command ("add" 'add "Add a new game to back up")
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

(define-command ("list" 'list-games "List games that can be backed up"))

(define-command ("info" 'info "Show detailed info for games that can be backed up")
  :free-args (("GAMES" :count :many)))

(define-command ("remove" 'remove-games "Remove games")
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

(define-command ("config" 'config "Edit program configuration")
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

(defun set-base-opts ()
  (opts:define-opts
    (:name :games-path
     :description "Path to games configuration file"
     :short #\g
     :long "games-path"
     :arg-parser #'identity
     :meta-var "GAMES_CONFIG_PATH")
    (:name :config-path
     :description "Path to sbu configuration file"
     :short #\c
     :long "config-path"
     :arg-parser #'identity
     :meta-var "PROGRAM_CONFIG_PATH")
    (:name :version
     :description "Print version information"
     :long "version")
    (:name :help
     :description "Print this help"
     :short #\h
     :long "help")))

(defun backup-file-callback (from to)
  (format t "~%~a ==>~%~4t~a" from to)
  (force-output))

(defparameter *day-names*
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter *month-names*
  '("Jan" "Feb" "Mar" "Apr" "May"
    "Jun" "Jul" "Aug" "Sep" "Oct"
    "Nov" "Dec"))

(defparameter *verbose* nil
  "Whether to print verbose output.")

(defun clean-up-callback (files)
  (when *verbose*
    (format *error-output* "~%Deleted old backup~p:~:*~[~; ~:;~%~]~{~a~%~}"
            (length files)
            files)))

(defvar *application-catch-errors* nil)

(defmacro sbu (&rest options)
  (apply #'main (unlispify-options options)))

(defun unlispify-options (options)
  (loop for opt in options
        collect (typecase opt
                  (keyword (format nil "~@[-~*~]-~a"
                                   (> (length (symbol-name opt)) 1)
                                   (string-downcase (symbol-name opt))))
                  (string opt)
                  (pathname (namestring opt))
                  (symbol (string-downcase opt))
                  (t (write-to-string opt)))))

(defun main (&rest args)
  (flet ((error-and-abort (condition debugger-hook)
           (declare (ignore debugger-hook))
           (format *error-output* "Error: ~a~%" condition)
           (return-from main)))
    (handler-case
        (let ((*debugger-hook* (if *application-catch-errors*
                                   #'error-and-abort
                                   *debugger-hook*))
              (*program-name* (file-namestring (or (first (uiop:raw-command-line-arguments))
                                                   *program-name*))))
          (set-base-opts)
          (let* ((full-args (or (and args (cons nil args))
                                (uiop:raw-command-line-arguments)))
                 (commands (commands))
                 (command-position (position-if (op (position _ commands :test #'string=))
                                                full-args))
                 (pre-command-args (if command-position
                                       (take command-position full-args)
                                       full-args)))
            (if command-position
                (let* ((options (opts:get-opts pre-command-args))
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
                      (describe-commands :prefix (version) :usage-of *program-name* :brief t)
                      (bind (((subcommand . opts) args))
                        (handle-command subcommand opts *program-name*))))
                (multiple-value-bind (opts free-args) (opts:get-opts (rest full-args))
                  (cond ((getf opts :version) (print-full-version-info))
                        ((first free-args) (error 'opt-commands:unknown-command :command (first free-args)))
                        (t (error 'unix-opts:missing-arg :option "COMMAND")))))))
      (opts:troublesome-option (condition)
        (opts:describe :usage-of *program-name*
                       :prefix (format nil "Error: ~a" condition)
                       :args "COMMAND"
                       :brief t)))))

(defun version ()
  (let ((version #.(asdf:component-version (asdf:find-system :sbu/cli))))
    (format nil "sbu v~a" version)))

(defun print-full-version-info ()
  (format *error-output* "~a~%Compiled ~a with ~a ~a~%~%"
          (version)
          #.(multiple-value-bind
                  (second minute hour date month year)
                (get-decoded-time)
              (format nil "on ~d-~2,'0d-~2,'0d at ~2,'0d:~2,'0d:~2,'0d"
                      year month date hour minute second))
          #.(lisp-implementation-type)
          #.(lisp-implementation-version)))

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
                            (cl-fad:pathname-as-directory
                             (tu:canonicalize-path (getf options :path)))
                            (or (getf options :glob) ""))
             (format *error-output* "Added game:~%~%")
             (print-game-info (cons game-name (@ games game-name)))))))

(defun list-games (options free-args)
  (declare (ignore options free-args))
  (let ((games (sbu:load-games)))
    (format t "~{~a~%~}~%"
            (sort (hash-table-keys games) #'string-lessp))))

(defun info (options free-args)
  (declare (ignore options))
  (let* ((games (sbu:load-games))
         (games-alist (sort (hash-table-alist games) #'string-lessp :key #'car)))
    (loop for game in games-alist
          when (or (null free-args)
                   (position (car game) free-args :test 'string=))
            do (print-game-info game))))

(tu:fn print-game-info ((game-name &key save-path save-glob)
                        &key new-game-name new-save-path new-save-glob)
  (format *error-output* "Name: ~a~@[ -> ~a~]
Save path: ~a~@[ -> ~a~]
~@[Save glob: ~a~@[ -> ~a~]~%~]~%"
          game-name new-game-name
          save-path new-save-path
          (when (or (> (length save-glob) 0)
                    new-save-glob)
            save-glob)
          new-save-glob))

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
         (new-game-path (when (getf options :path)
                          (cl-fad:pathname-as-directory
                           (tu:canonicalize-path (getf options :path)))))
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
             (print-game-info `(,old-game-name :save-path ,old-game-path
                                               :save-glob ,old-game-glob)
                              :new-game-name (getf options :name)
                              :new-save-path new-game-path
                              :new-save-glob new-game-glob)))))

(defun config (options free-args)
  (declare (ignore free-args))
  (let* ((config (sbu:load-config))
         (old-backup-path (or (@ config :backup-path)
                              sbu:*backup-path*))
         (old-backup-frequency (or (@ config :backup-frequency)
                                   sbu:*backup-frequency*))
         (old-backups-to-keep (or (@ config :backups-to-keep)
                                  sbu:*backups-to-keep*))
         (new-backup-path (when (getf options :path)
                            (cl-fad:pathname-as-directory
                             (tu:canonicalize-path (getf options :path)))))
         (new-backup-frequency (getf options :frequency))
         (new-backups-to-keep (getf options :keep)))
    (when (or new-backup-path new-backup-frequency new-backups-to-keep)
      (sbu:save-config (dict :backup-path (or new-backup-path old-backup-path)
                             :backup-frequency (or new-backup-frequency old-backup-frequency)
                             :backups-to-keep (or new-backups-to-keep old-backups-to-keep))))
    (format *error-output* "Backup path: ~a~@[ -> ~a~]
Backup frequency (in minutes): ~a~@[ -> ~a~]
Number of backups to keep: ~a~@[ -> ~a~]~%~%"
            old-backup-path new-backup-path
            old-backup-frequency new-backup-frequency
            old-backups-to-keep new-backups-to-keep)))
