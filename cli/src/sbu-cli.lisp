(defpackage :sbu/cli
  (:use #:cl
        #:serapeum
        #:metabang-bind
        #:alexandria)
  (:export #:main))

(in-package :sbu/cli)

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

;; (define-command "config" #'config
;;   (opts:define-opts
;;     (:name :path
;;      :description "Path to directory in which to back up saves"
;;      :short #\p
;;      :long "path"
;;      :arg-parser #'identity
;;      :meta-var "BACKUP_PATH")
;;     (:name :frequency
;;      :description "Frequency in minutes to backup saves when looping"
;;      :short #\f
;;      :long "frequency"
;;      :arg-parser #'parse-integer
;;      :meta-var "BACKUP_FREQUENCY")
;;     (:name :keep
;;      :description "How many copies of each backed-up file to keep"
;;      :short #\k
;;      :long "keep"
;;      :arg-parser #'parse-integer
;;      :meta-var "BACKUPS_TO_KEEP")))

(define-condition args-error (error)
  ((error-string :initarg :error-string :reader error-string))
  (:report (lambda (condition stream)
             (format stream (error-string condition)))))

(defun describe-commands (&key prefix suffix usage-of)
  "Print the help screen showing which commands are available.

`prefix' will be printed before the list of available commands.
`suffix' will be printed after the list of available commands.
`usage-of' take the name of the application and uses it to show
how the commands are used."
  (format t "~@[~a~%~]~@[~%Usage: ~a COMMAND~%~]" prefix usage-of)
  (~>> *commands*
       hash-table-alist
       (sort _ #'string-lessp :key #'car)
       (mapcar (op (list (car _1) (getf (cdr _1) :description))))
       (format t "~%Available commands:~%~:{~2t~16a~a~%~}"))
  (format t "~@[~%~a~%~%~]" suffix))

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

(defun handle-command (command args &optional application-name)
  (if (set-opts command)
      (if (help-flag-p args)
          (opts:describe :usage-of (when application-name
                                     (format nil "~a ~a" application-name command))
                         :args (get-command-free-args command))
          (handler-case
              (bind ((command-function (get-command-function command))
                     ((:values options free-args) (when args (opts:get-opts args))))
                (funcall command-function options free-args))
            (args-error (condition)
              (opts:describe :usage-of (when application-name
                                         (format nil "~a ~a" application-name command))
                             :args (get-command-free-args command)
                             :prefix (format nil "Error: ~a" condition)))))
      (describe-commands :usage-of application-name)))

(defun main (&rest args)
  (handler-case
      (let ((args (or (and args (cons nil args))
                      (tu:get-command-line-args))))
        (if (= 1 (length args))
            (describe-commands :usage-of "sbu")
            (bind (((_ subcommand . opts) args))
              (handle-command subcommand opts "sbu"))))
    (error (condition)
      (format *error-output* "Error: ~a" condition))))

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
        (return)))))

(defun add (options free-args)
  (let* ((games (sbu:load-games))
         (game-names (hash-table-keys games))
         (game-name (first free-args)))
    (cond ((null free-args)
           (error 'args-error :error-string "Name of game to add not provided."))
          ((> (length free-args) 1)
           (error 'args-error
                  :error-string (format nil "Too many arguments provided.  Extra arguments: ~{~a~^, ~}"
                                        (rest free-args))))
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
           (error 'args-error
                  :error-string "No games provided to remove."))
          ((and (null game-names) free-args)
           (error "Games don't exist: ~{~a~^, ~}" free-args))
          ((or (getf options :yes)
               (progn (format t "Are you sure you'd like to remove the following games from sbu?
~{~a~^, ~}~%"
                              free-args)
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
    (cond ((not old-game-name) (error 'args-error
                                      :error-string "Name of game to edit not provided."))
          ((not old-game) (error "~a doesn't exist.  Use `add' command to add it."
                                 old-game-name))
          ((> (length free-args) 1)
           (error 'args-error
                  :error-string (format nil
                                        "Too many arguments provided.  Extra arguments: ~{~a~^, ~}"
                                        (rest free-args))))
          ((and (string/= old-game-name new-game-name)
                (position new-game-name game-names :test #'string=))
           (error "Can't rename to ~a: already exists." new-game-name))
          ((not (or (getf options :name) new-game-path new-game-glob))
           (error 'args-error
                  :error-string "At least one of name, path, or glob must be provided"))
          (t (sbu:save-game games
                            new-game-name
                            (or new-game-path old-game-path)
                            (or new-game-glob old-game-glob "")
                            old-game-name)
             (format t "~@[Name: ~{~a -> ~a~}~%~]~
~@[Save-Path: ~{~a -> ~a~}~%~]~
~@[Save-Glob: ~{~a -> ~a~}~%~]~%"
                     (and (getf options :name) (list old-game-name new-game-name))
                     (and new-game-path (list old-game-path new-game-path))
                     (and new-game-glob (list old-game-glob new-game-glob)))))))
