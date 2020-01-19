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
           #:*backups-to-keep*

           #:sbu-error
           #:load-config-error
           #:bad-config-format-error
           #:save-config-error
           #:backup-game-error
           #:backup-file-error
           #:clean-up-error

           #:skip-game
           #:skip-file
           #:skip-clean-up

           #:backup-complete
           #:file-copied))

(in-package :sbu)

(defparameter *games-path* "~/.sbugames")
(defparameter *config-path* "~/.sbuconfig")
(defparameter *backup-path* "~/.sbu-backups/")
(defparameter *backup-frequency* 15)
(defparameter *backups-to-keep* 10)

(define-condition sbu-error (error)
  ())

(define-condition save-config-error (sbu-error file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unable to save config to ~a"
                     (file-error-pathname condition)))))

(define-condition bad-config-format-error (sbu-error file-error)
  ((contents :initarg :contents :reader bad-config-format-error-contents))
  (:report (lambda (condition stream)
             (format stream "Config file at ~a has invalid format:~%~a"
                     (file-error-pathname condition)
                     (bad-config-format-error-contents condition)))))

(define-condition load-config-error (sbu-error file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Could not load config from ~a"
                     (file-error-pathname condition)))))

(defun save-games (games)
  (save-config games *games-path*))

(defun load-games ()
  (load-config *games-path*))

(defun save-config (config &optional (pathname *config-path*))
  (handler-case
      (let ((game-alist (hash-table-alist config)))
        (with-open-file (out pathname
                             :direction :output
                             :if-exists :supersede)
          (with-standard-io-syntax
            (pprint game-alist out)))
        game-alist)
    (error ()
      (error 'save-config-error :path pathname))))

(defun load-config (&optional (pathname *config-path*))
  (handler-case
      (if (probe-file pathname)
          (with-open-file (in pathname)
            (with-standard-input-syntax
              (alist-hash-table (read in) :test 'equal)))
          (make-hash-table :test 'equal))
    (stream-error ()
      (error 'bad-config-format-error
             :contents (read-file-into-string pathname)
             :pathname pathname))
    (error ()
      (error 'load-config-error :pathname pathname))))

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

(define-condition backup-error (sbu-error)
  ((game-name :initarg :game-name :reader backup-error-game-name))
  (:report (lambda (condition stream)
             (format stream "Unable to back up ~a"
                     (backup-error-game-name condition)))))

(define-condition backup-game-error (backup-error)
  ((game-path :initarg :game-path :reader backup-game-error-game-path))
  (:report (lambda (condition stream)
             (format stream "Unable to back up game ~a with save path ~a."
                     (backup-error-game-name condition)
                     (backup-game-error-game-path condition)))))

(defun ignore-file (condition)
  (declare (ignore condition))
  (invoke-restart 'ignore-file))

(defun skip-game (condition)
  (declare (ignore condition))
  (invoke-restart 'skip-game))

(define-condition backup-complete ()
  ((game-name :initarg :game-name :reader backup-complete-game-name)
   (finish-time :initarg :finish-time :reader backup-complete-finish-time)
   (seconds-passed :initarg :seconds-passed :reader backup-complete-seconds-passed))
  (:report (lambda (condition stream)
             (bind (((:values second minute hour date month year day-of-week _ tz)
                     (decode-universal-time (backup-complete-finish-time condition))))
               (format stream "Finished backing up ~a in ~fs ~
on ~a, ~a ~d ~d at ~2,'0d:~2,'0d:~2,'0d (GMT~@d)"
                       (backup-complete-game-name condition)
                       (backup-complete-seconds-passed condition)
                       (nth day-of-week *day-names*)
                       (nth month *month-names*)
                       date
                       year
                       hour
                       minute
                       second
                       (- tz))))))

(tu:desfun backup-game ((game-name . (&key save-path save-glob)))
  (restart-case
      (handler-case
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
            (bind ((end-time (get-internal-real-time)))
              (signal 'backup-complete
                      :game-name game-name
                      :seconds-passed (/ (- end-time start-time)
                                         internal-time-units-per-second)
                      :finish-time (get-universal-time))))
        (error ()
          (error 'backup-game-error :game-name game-name
                                    :game-path save-path)))
    (skip-game () nil)))

(define-condition backup-file-error (backup-error file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unable to back up file ~a for ~a"
                     (file-error-pathname condition)
                     (backup-error-game-name condition)))))

(defun skip-file (condition)
  (declare (ignore condition))
  (invoke-restart 'skip-file))

(define-condition file-copied ()
  ((from :initarg :from :reader file-copied-from)
   (to :initarg :to :reader file-copied-to))
  (:report (lambda (condition stream)
             (format stream "~a ==>~%~4t~a"
                     (file-copied-from condition)
                     (file-copied-to condition)))))

(defun backup-file (game-name save-path from)
  (restart-case
      (handler-case
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
              (signal 'file-copied :from from :to to))
            (clean-up to))
        (file-error (condition)
          (error condition))
        (error ()
          (error 'backup-file-error :game-name game-name
                                    :pathname from)))
    (skip-file () nil)))

(define-condition clean-up-error (sbu-error file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Could not clean up extra backups for ~a"
                     (file-error-pathname condition)))))

(defun skip-clean-up (condition)
  (declare (ignore condition))
  (invoke-restart 'skip-clean-up))

(defun clean-up (file-path)
  (restart-case
      (handler-case
          (let ((files (directory (string+ file-path ".bak.*_*_*_*_*_*"))))
            (when (> (length files) *backups-to-keep*)
              (let ((files-to-delete (drop *backups-to-keep*
                                           (sort files (lambda (f1 f2)
                                                         (> (file-write-date f1)
                                                            (file-write-date f2)))))))
                (mapcar #'delete-file files-to-delete)
                (format t "~%Deleted old backup~p:~:*~[~; ~:;~%~]~{~a~%~}"
                        (length files-to-delete)
                        files-to-delete))))
        (error ()
          (error 'clean-up-error :pathname file-path)))
    (skip-clean-up () nil)))

(defun save-game (games game-name game-save-path game-save-glob &optional old-game-name)
  (remhash old-game-name games)
  (setf (gethash game-name games) `(:save-path ,game-save-path
                                    :save-glob ,game-save-glob))
  (save-games games))

(defun remove-game (games game-name)
  (remhash game-name games)
  (save-games games))
