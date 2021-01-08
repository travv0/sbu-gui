(defpackage #:sbu/cli/tests
  (:use #:cl #:5am #:mockingbird)
  (:import-from #:sbu/tests/utils #:fs-mock)
  (:local-nicknames (#:tu #:travv0.utils))
  (:export #:run-tests #:generate-coverage #:sbu/cli))

(in-package #:sbu/cli/tests)

(def-suite sbu/cli)
(in-suite sbu/cli)

(defun run-tests ()
  (run! 'sbu/cli))

(defun program-name ()
  (file-namestring (first (uiop:raw-command-line-arguments))))

(defun remove-whitespace (s)
  (remove-if #'serapeum:whitespacep s))

(defun generate-coverage (directory)
  (sb-cover:clear-coverage)
  (declaim (optimize sb-cover:store-coverage-data))
  (asdf:oos 'asdf:load-op :sbu/cli :force t)
  (run-tests)
  (sb-cover:report (fad:pathname-as-directory directory))
  (declaim (optimize (sb-cover:store-coverage-data 0)))
  (sb-cover:clear-coverage))

(test add-game
  (with-fixture fs-mock ()
    (signals simple-error
      (sbu/cli::add '(:path "~/test") '("test")))
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli::add '(:path "~/new" :glob "*") '("new")))))
      (is (string= (format nil "Added game:

Name: new
Save path: ~a
Save glob: *

"
                           (tu:canonicalize-path "~/new/"))
                   s)))
    (let ((games (sbu:load-games)))
      (is (= 3 (hash-table-count games)))
      (is (equal `(:save-path ,(tu:canonicalize-path "~/new/")
                   :save-glob "*")
                 (gethash "new" games))))))

(test list-games
  (with-fixture fs-mock ()
    (let ((s (with-output-to-string (*standard-output*)
               (sbu/cli::list-games '() '()))))
      (is (string= "another
test

"
                   s)))))

(test print-game-info
  (with-fixture fs-mock ()
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli::info '() '("another")))))
      (is (string= "Name: another
Save path: /another/

"
                   s)))
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli::info '() '()))))
      (is (string= "Name: another
Save path: /another/

Name: test
Save path: /test/path/

"
                   s)))))

(test edit-game
  (with-fixture fs-mock ()
    (signals simple-error
      (sbu/cli::edit '(:path "~/new" :glob "*") '("new")))
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli::edit '(:path "~/test" :glob "*") '("test")))))
      (is (string= (format nil "Name: test
Save path: /test/path/ -> ~a
Save glob:  -> *

"
                           (tu:canonicalize-path "~/test/"))
                   s)))
    (let ((games (sbu:load-games)))
      (is (= 2 (hash-table-count games)))
      (is (equal `(:save-path ,(tu:canonicalize-path "~/test/")
                   :save-glob "*")
                 (gethash "test" games))))))

(test remove-game
  (with-fixture fs-mock ()
    (signals simple-error
      (sbu/cli::remove-games '(:yes t) '("new")))
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli::remove-games '(:yes t) '("another")))))
      (is (string= "Removed the following games: another

"
                   s)))
    (let ((games (sbu:load-games)))
      (is (= 1 (hash-table-count games)))
      (is (null (gethash "another" games))))))

(test config
  (with-fixture fs-mock ()
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli::config '() '()))))
      (is (string= (format nil "Backup path: ~a
Backup frequency (in minutes): 13
Number of backups to keep: 10

"
                           (tu:canonicalize-path "~/.sbu-backups/"))
                   s)))
    (let ((config (sbu:load-config)))
      (is (equalp (serapeum:dict
                   :backups-to-keep 10
                   :backup-frequency 13
                   :backup-path (tu:canonicalize-path "~/.sbu-backups/"))
                  config)))

    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli::config '(:path "~/diff-path" :keep 4) '()))))
      (is (string= (format nil "Backup path: ~a -> ~a
Backup frequency (in minutes): 13
Number of backups to keep: 10 -> 4

"
                           (tu:canonicalize-path "~/.sbu-backups/")
                           (tu:canonicalize-path "~/diff-path/"))
                   s)))
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli::config '(:frequency 10) '()))))
      (is (string= (format nil "Backup path: ~a
Backup frequency (in minutes): 13 -> 10
Number of backups to keep: 4

"
                           (tu:canonicalize-path "~/diff-path/"))
                   s)))
    (let ((config (sbu:load-config)))
      (is (equalp (serapeum:dict
                   :backups-to-keep 4
                   :backup-frequency 10
                   :backup-path (tu:canonicalize-path "~/diff-path/"))
                  config)))))

(test cli-no-args
  (with-fixture fs-mock ()
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main))))
      (is (string= (remove-whitespace
                    (format nil "Error: missing arg for option: \"COMMAND\"

Usage: ~a [-g|--games-path GAMES_CONFIG_PATH]
           [-c|--config-path PROGRAM_CONFIG_PATH] [--version] [-h|--help] COMMAND

"
                            (program-name)))
                   (remove-whitespace s))))))

(test cli-bad-command
  (with-fixture fs-mock ()
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "asdf"))))
      (is (string= (remove-whitespace
                    (format nil "Error: unknown command: \"asdf\"

Did you mean this?
        add

Usage: ~a [-g|--games-path GAMES_CONFIG_PATH]
           [-c|--config-path PROGRAM_CONFIG_PATH] [--version] [-h|--help] COMMAND

"
                            (program-name)))
                   (remove-whitespace s))))))

(test cli-bad-but-close-opts
  (with-fixture fs-mock ()
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "backu"))))
      (is (string= (remove-whitespace
                    (format nil "Error: unknown command: \"backu\"

Did you mean this?
        backup

Usage: ~a [-g|--games-path GAMES_CONFIG_PATH]
           [-c|--config-path PROGRAM_CONFIG_PATH] [--version] [-h|--help] COMMAND

"
                            (program-name)))
                   (remove-whitespace s))))

    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "-a" "backu"))))
      (is (string= (remove-whitespace
                    (format nil "Error: unknown option: \"-a\"

Did you mean one of these?
        -g
        -c
        -h

Usage: ~a [-g|--games-path GAMES_CONFIG_PATH]
           [-c|--config-path PROGRAM_CONFIG_PATH] [--version] [-h|--help] COMMAND

"
                            (program-name)))
                   (remove-whitespace s))))

    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "backup" "-a"))))
      (is (string= (remove-whitespace
                    (format nil "Error: unknown option: \"-a\"

Did you mean one of these?
        -l
        -v
        -h

Usage: ~a backup [-l|--loop] [-v|--verbose] [-h|--help] GAMES...

"
                            (program-name)))
                   (remove-whitespace s))))

    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "backup" "--hepl"))))
      (is (string= (remove-whitespace
                    (format nil "Error: unknown option: \"--hepl\"

Did you mean this?
        --help

Usage: ~a backup [-l|--loop] [-v|--verbose] [-h|--help] GAMES...

"
                            (program-name)))
                   (remove-whitespace s))))

    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "--hepl"))))
      (is (string= (remove-whitespace
                    (format nil "Error: unknown option: \"--hepl\"

Did you mean this?
        --help

Usage: ~a [-g|--games-path GAMES_CONFIG_PATH]
           [-c|--config-path PROGRAM_CONFIG_PATH] [--version] [-h|--help] COMMAND

"
                            (program-name)))
                   (remove-whitespace s))))))

(test cli-help
  (with-fixture fs-mock ()
    (let ((help-output (remove-whitespace
                        (format nil "~a

Usage: ~a [-g|--games-path GAMES_CONFIG_PATH]
                [-c|--config-path PROGRAM_CONFIG_PATH] [--version] [-h|--help] COMMAND

Available options:
  -g, --games-path GAMES_CONFIG_PATH
                            Path to games configuration file
  -c, --config-path PROGRAM_CONFIG_PATH
                            Path to sbu configuration file
  --version                 Print version information
  -h, --help                Print this help

Available commands:
  add                       Add a new game to back up
  backup                    Back up your games
  config                    Edit program configuration
  edit                      Edit game info.
  info                      Show detailed info for games that can be backed up
  list                      List games that can be backed up
  remove                    Remove games


"
                                (sbu/cli::version)
                                (program-name)))))
      (let ((s (with-output-to-string (*error-output*)
                 (sbu/cli:main "--help"))))
        (is (string= help-output (remove-whitespace s))))
      (let ((s (with-output-to-string (*error-output*)
                 (sbu/cli:main "--help" "add"))))
        (is (string= help-output (remove-whitespace s)))))))

(test cli-version
  (with-fixture fs-mock ()
    (let ((version-output (with-output-to-string (*error-output*)
                            (sbu/cli::print-full-version-info))))
      (let ((s (with-output-to-string (*error-output*)
                 (sbu/cli:main "--version"))))
        (is (string= version-output s)))
      (let ((s (with-output-to-string (*error-output*)
                 (sbu/cli:main "--version" "add"))))
        (is (string= version-output s))))))

(test cli-add
  (with-fixture fs-mock ()
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "add"))))
      (is (string= (remove-whitespace
                    (format nil "Error: missing arguments: \"GAME\"

Usage: ~a add [-p|--path GAME_SAVE_PATH (Required)]
                    [-g|--glob GAME_SAVE_FILE_GLOB] [-h|--help] GAME

"
                            (program-name)))
                   (remove-whitespace s))))
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "add" "--path" "asdf"))))
      (is (string= (remove-whitespace
                    (format nil "Error: missing arguments: \"GAME\"

Usage: ~a add [-p|--path GAME_SAVE_PATH (Required)]
                    [-g|--glob GAME_SAVE_FILE_GLOB] [-h|--help] GAME

"
                            (program-name)))
                   (remove-whitespace s))))
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "add" "fdsa" "--path" "asdf"))))
      (is (string= (format nil "Added game:

Name: fdsa
Save path: ~a

"
                           (tu:canonicalize-path "asdf/"))
                   s)))))

(test cli-config
  (with-fixture fs-mock ()
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "config"))))
      (is (string= (format nil "Backup path: ~a
Backup frequency (in minutes): 13
Number of backups to keep: 10

"
                           (tu:canonicalize-path "~/.sbu-backups/"))
                   s)))
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "config" "-p" "~/diff-path" "-k" "4"))))
      (is (string= (format nil "Backup path: ~a -> ~a
Backup frequency (in minutes): 13
Number of backups to keep: 10 -> 4

"
                           (tu:canonicalize-path "~/.sbu-backups/")
                           (tu:canonicalize-path "~/diff-path/"))
                   s)))
    (let ((s (with-output-to-string (*error-output*)
               (sbu/cli:main "config" "--help"))))
      (is (string= (remove-whitespace
                    (format nil "
Usage: ~a config [-p|--path BACKUP_PATH] [-f|--frequency BACKUP_FREQUENCY]
                       [-k|--keep BACKUPS_TO_KEEP] [-h|--help]

Available options:
  -p, --path BACKUP_PATH    Path to directory in which to back up saves
  -f, --frequency BACKUP_FREQUENCY
                            Frequency in minutes to backup saves when looping
  -k, --keep BACKUPS_TO_KEEP
                            How many copies of each backed-up file to keep
  -h, --help                Print this help

"
                            (program-name)))
                   (remove-whitespace s))))))

(test cli-backup
  (with-fixture fs-mock ()
    (let ((backup-directory (fad:merge-pathnames-as-directory
                             (asdf:system-source-directory :sbu/cli/tests)
                             "cli/t/backups/"))
          (save-directory (fad:merge-pathnames-as-directory
                           (asdf:system-source-directory :sbu/cli/tests)
                           "cli/t/test-files/")))
      (with-output-to-string (*error-output*)
        (sbu/cli:main "config" "-p" (namestring backup-directory))
        (sbu/cli:main "add" "test-game" "-p" (namestring save-directory)))
      (unwind-protect
           (progn
             (let ((stderr (make-array 0 :element-type 'character :fill-pointer t :adjustable t))
                   (stdout (make-array 0 :element-type 'character :fill-pointer t :adjustable t)))
               (with-output-to-string (*error-output* stderr)
                 (with-output-to-string (*standard-output* stdout)
                   (sbu/cli:main "backup" "test-game")))
               (is (string= (format nil "
~aa.txt ==>
    ~atest-game/a.txt
~ab.txt ==>
    ~atest-game/b.txt
~ac.txt ==>
    ~atest-game/c.txt"
                                    save-directory backup-directory
                                    save-directory backup-directory
                                    save-directory backup-directory)
                            stdout))
               (is-true (search "Finished backing up 3 files for test-game in" stderr)))
             (is (= 6 (length (fad:list-directory (fad:merge-pathnames-as-directory
                                                   backup-directory
                                                   "test-game/")))))
             (let ((stderr (make-array 0 :element-type 'character :fill-pointer t :adjustable t))
                   (stdout (make-array 0 :element-type 'character :fill-pointer t :adjustable t)))
               (with-output-to-string (*error-output* stderr)
                 (with-output-to-string (*standard-output* stdout)
                   (sbu/cli:main "backup" "test-game")))
               (is (uiop:emptyp stdout))
               (is (uiop:emptyp stderr)))
             (is (= 6 (length (fad:list-directory (fad:merge-pathnames-as-directory
                                                   backup-directory
                                                   "test-game/"))))))
        (uiop:delete-directory-tree backup-directory :validate (constantly t))))))
