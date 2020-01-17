(defpackage :sbu/cli
  (:use #:cl
        #:serapeum
        #:metabang-bind
        #:alexandria)
  (:export #:main))

(in-package :sbu/cli)

(defparameter *commands* (dict))
(defmacro define-command ((command-name function &optional description) &body opts)
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

(defun describe-commands (&key prefix suffix usage-of)
  (format t "~@[~a~%~]" prefix)
  (format t "~@[~%Usage: ~a COMMAND~%~]" usage-of)
  (~>> *commands*
       hash-table-alist
       (sort _ #'string-lessp :key #'car)
       (mapcar (op (list (car _1) (getf (cdr _1) :description))))
       (format t "~%Available commands:~%~:{~2t~16a~a~%~}"))
  (format t "~@[~%~a~%~%~]" suffix))

(defun set-opts (command)
  (funcall (getf (@ *commands* command) :make-opts)))

(defun get-command-function (command)
  (getf (@ *commands* command) :function))

(defun get-command-free-args (command)
  (getf (@ *commands* command) :free-args))

(defun main (&optional args)
  (let ((args (or (and args (cons nil args))
                  (tu:get-command-line-args))))
    (if (> (length args) 1)
        (bind (((_ subcommand . opts) args))
          (set-opts subcommand)
          (if (or (position "-h" opts :test 'string=)
                  (position "--help" opts :test 'string=))
              (opts:describe :usage-of (format nil "sbu ~a" subcommand)
                             :args (get-command-free-args subcommand))
              (handler-case
                  (bind ((command-function (get-command-function subcommand))
                         ((:values options free-args) (when opts (opts:get-opts opts))))
                    (funcall command-function options free-args))
                (error (condition)
                  (opts:describe :usage-of (format nil "sbu ~a" subcommand)
                                 :args (get-command-free-args subcommand)
                                 :prefix (format nil "Error: ~a" condition))
                  (opts:exit 1)))))
        (describe-commands :usage-of "sbu"))))

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
    (cond ((null free-args) (error "Name of game to add not provided."))
          ((> (length free-args) 1)
           (error "Too many arguments provided.  Extra arguments: ~{~a~^, ~}"
                  (rest free-args)))
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
         (game-names (remove-if-not (op (position _ existing-game-names :test #'string=))
                                    free-args)))
    (cond ((null game-names)
           (error "~[No games provided to remove.~;Games don't exist: ~{~a~^, ~}~]"
                  (if (null free-args) 0 1)
                  free-args))
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
         (new-game-name (or (getf options :name) old-game-name)))
    (cond ((not old-game) (error "~a doesn't exist.  Use `add' command to add it." old-game-name))
          ((null free-args) (error "Name of game to edit not provided."))
          ((> (length free-args) 1)
           (error "Too many arguments provided.  Extra arguments: ~{~a~^, ~}"
                  (rest free-args)))
          ((and (string/= old-game-name new-game-name)
                (position new-game-name game-names :test #'string=))
           (error "Can't rename to ~a: already exists." new-game-name))
          (t (sbu:save-game games
                            new-game-name
                            (or (getf options :path) (getf old-game :save-path))
                            (or (getf options :glob) (getf old-game :save-glob) "")
                            old-game-name)
             (format t "Updated ~a.~%" new-game-name)))))
