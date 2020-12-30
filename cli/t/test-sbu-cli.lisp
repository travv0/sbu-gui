(defpackage #:sbu/cli/tests
  (:use #:cl #:5am #:mockingbird)
  (:local-nicknames (#:tu #:travv0.utils)
                    (#:a #:alexandria))
  (:export #:run-tests))

(in-package #:sbu/cli/tests)

(def-suite sbu/cli)
(in-suite sbu/cli)

(defun run-tests ()
  (run! 'sbu/cli))

(defvar *config-storage*)
(defvar *games-storage*)

(defparameter *default-config*
  (serapeum:dict
   :backups-to-keep 10
   :backup-frequency 13
   :backup-path (tu:canonicalize-path "~/.sbu-backups/")))

(defparameter *default-games*
  (serapeum:dict
   "test" '(:save-path #p"/test/path/" :save-glob "")
   "another" '(:save-path #p"/another/" :save-glob "")))

(defun load-games-mock (&rest rest)
  (declare (ignore rest))
  (a:copy-hash-table (or *games-storage* *default-games*)))

(defun save-games-mock (games &rest rest)
  (declare (ignore rest))
  (setf *games-storage* games))

(defun load-config-mock (&rest rest)
  (declare (ignore rest))
  (a:copy-hash-table (or *config-storage* *default-config*)))

(defun save-config-mock (config &rest rest)
  (declare (ignore rest))
  (setf *config-storage* config))

(test add-game
  (with-dynamic-stubs ((sbu:save-games #'save-games-mock)
                       (sbu:load-games #'load-games-mock))
    (let (*games-storage*)
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
                   (gethash "new" games)))))))

(test list-games
  (with-dynamic-stubs ((sbu:load-games #'load-games-mock))
    (let (*games-storage*)
      (let ((s (with-output-to-string (*standard-output*)
                 (sbu/cli::list-games '() '()))))
        (is (string= "another
test

"
                     s))))))

(test print-game-info
  (with-dynamic-stubs ((sbu:load-games #'load-games-mock))
    (let (*games-storage*)
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
                     s))))))

(test edit-game
  (with-dynamic-stubs ((sbu:save-games #'save-games-mock)
                       (sbu:load-games #'load-games-mock))
    (let (*games-storage*)
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
                   (gethash "test" games)))))))

(test remove-game
  (with-dynamic-stubs ((sbu:save-games #'save-games-mock)
                       (sbu:load-games #'load-games-mock))
    (let (*games-storage*)
      (signals simple-error
        (sbu/cli::remove-games '(:yes t) '("new")))
      (let ((s (with-output-to-string (*error-output*)
                 (sbu/cli::remove-games '(:yes t) '("another")))))
        (is (string= "Removed the following games: another

"
                     s)))
      (let ((games (sbu:load-games)))
        (is (= 1 (hash-table-count games)))
        (is (null (gethash "another" games)))))))

(test config
  (with-dynamic-stubs ((sbu:save-config #'save-config-mock)
                       (sbu:load-config #'load-config-mock))
    (let (*config-storage*)
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
                    config))))))
