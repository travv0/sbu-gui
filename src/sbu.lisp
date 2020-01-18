(defpackage :sbu
  (:use #:cl #:alexandria #:serapeum #:metabang-bind)
  (:export #:save-games
           #:load-games
           #:save-config
           #:load-config
           #:backup-all
           #:backup-game
           #:save-game
           #:remove-game
           #:*games-path*
           #:*config-path*
           #:*backup-path*
           #:*backup-frequency*
           #:*backups-to-keep*))

(in-package :sbu)

(defparameter *games-path* "~/.sbugames")
(defparameter *config-path* "~/.sbuconfig")
(defparameter *backup-path* "~/.sbu-backups/")
(defparameter *backup-frequency* 15)
(defparameter *backups-to-keep* 10)

(defun save-games (games)
  (let ((game-alist (hash-table-alist games)))
    (with-open-file (out *games-path*
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (pprint game-alist out)))
    game-alist))

(defun load-games ()
  (if (probe-file *games-path*)
      (with-open-file (in *games-path*)
        (with-standard-input-syntax
          (alist-hash-table (read in) :test 'equal)))
      (make-hash-table :test 'equal)))

(defun save-config (config)
  (let ((config-alist (hash-table-alist config)))
    (with-open-file (out *config-path*
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (pprint config-alist out)))
    config-alist))

(defun load-config ()
  (if (probe-file *config-path*)
      (with-open-file (in *config-path*)
        (with-standard-input-syntax
          (alist-hash-table (read in) :test 'equal)))
      (make-hash-table :test 'equal)))

(defun backup-all (games)
  (~>> games
       hash-table-alist
       (mapcar 'backup-game)))

(defparameter *day-names*
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter *month-names*
  '("Jan" "Feb" "Mar" "Apr" "May"
    "Jun" "Jul" "Aug" "Sep" "Oct"
    "Nov" "Dec"))

(tu:desfun backup-game ((game-name . (&key save-path save-glob)))
  (let ((start-time (get-internal-real-time)))
    (cl-fad:walk-directory save-path
                           (curry 'backup-file game-name save-path)
                           :directories :depth-first
                           :follow-symlinks nil
                           :test (lambda (file)
                                   (and (not (cl-fad:directory-pathname-p file))
                                        (pathname-match-p (cl-fad:pathname-as-file file)
                                                          (path:catfile
                                                           (cl-fad:pathname-as-directory save-path)
                                                           (if (string= (or save-glob "") "")
                                                               "**/*"
                                                               save-glob))))))
    (bind ((end-time (get-internal-real-time))
           ((:values second minute hour date month year day-of-week _ tz)
            (get-decoded-time)))
      (format t "~%Finished backing up ~a in ~fs ~
on ~a, ~a ~d ~d at ~2,'0d:~2,'0d:~2,'0d (GMT~@d)~%~%"
              game-name
              (/ (- end-time start-time)
                 internal-time-units-per-second)
              (nth day-of-week *day-names*)
              (nth month *month-names*)
              date
              year
              hour
              minute
              second
              (- tz)))))

(defun backup-file (game-name save-path from)
  (bind ((save-path (cl-fad:pathname-as-directory save-path))
         (backup-path (path:catdir *backup-path* (cl-fad:pathname-as-directory game-name)))
         (relative-save-path (subseq (namestring from) (length (namestring save-path))))
         (to (path:catfile backup-path relative-save-path))
         ((:values sec min hour day month year) (decode-universal-time (or (file-write-date from) 0) 0))
         (full-to (format nil
                          "~a.bak.~4,'0d_~2,'0d_~2,'0d_~2,'0d_~2,'0d_~2,'0d"
                          to year month day hour min sec)))
    (unless (probe-file full-to)
      (ensure-directories-exist (path:dirname full-to))
      (cl-fad:copy-file from to :overwrite t)
      (cl-fad:copy-file to full-to)
      (format t "~%~a ==>~%~4t~a" from to))
    (clean-up to)))

(defun clean-up (file-path)
  (let ((files (directory (string+ file-path ".bak.*_*_*_*_*_*"))))
    (when (> (length files) *backups-to-keep*)
      (let ((files-to-delete (drop *backups-to-keep*
                                   (sort files (lambda (f1 f2)
                                                 (> (file-write-date f1)
                                                    (file-write-date f2)))))))
        (mapcar #'delete-file files-to-delete)
        (format t "~%Deleted old backup~p:~:*~[~; ~:;~%~]~{~a~%~}"
                (length files-to-delete)
                files-to-delete)))))

(defun save-game (games game-name game-save-path game-save-glob &optional old-game-name)
  (remhash old-game-name games)
  (setf (gethash game-name games) `(:save-path ,game-save-path
                                    :save-glob ,game-save-glob))
  (save-games games))

(defun remove-game (games game-name)
  (remhash game-name games)
  (save-games games))
