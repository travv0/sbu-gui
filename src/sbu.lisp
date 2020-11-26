(defpackage :sbu
  (:use #:cl #:metabang-bind #:defstar)
  (:local-nicknames (#:tu #:travv0.utils))
  (:import-from #:serapeum #:~> #:~>> #:op)
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

           #:*backup-file-callback*
           #:*backup-game-callback*
           #:*clean-up-callback*

           #:sbu-error
           #:load-config-error
           #:bad-config-format-error
           #:save-config-error
           #:backup-game-error
           #:backup-file-error
           #:clean-up-error

           #:skip-game
           #:treat-backup-as-complete
           #:skip-file
           #:treat-file-as-copied
           #:skip-clean-up))

(setf defstar:*check-argument-types-explicitly?* t)

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

(defun* (save-games -> list) ((games hash-table))
  (save-config games *games-path*))

(defun* (load-games -> hash-table) ()
  (load-config *games-path*))

(defun* (save-config -> list) ((config hash-table)
                               &optional ((pathname (or string pathname)) *config-path*))
  (handler-case
      (let ((game-alist (alexandria:hash-table-alist config)))
        (with-open-file (out pathname
                             :direction :output
                             :if-exists :supersede)
          (with-standard-io-syntax
            (pprint game-alist out)))
        game-alist)
    (error ()
      (error 'save-config-error :path pathname))))

(defun* (load-config -> hash-table) (&optional ((pathname (or string pathname)) *config-path*))
  (handler-case
      (if (probe-file pathname)
          (with-open-file (in pathname)
            (serapeum:with-standard-input-syntax
              (alexandria:alist-hash-table (read in) :test 'equal)))
          (make-hash-table :test 'equal))
    (stream-error ()
      (error 'bad-config-format-error
             :contents (alexandria:read-file-into-string pathname)
             :pathname pathname))
    (error ()
      (error 'load-config-error :pathname pathname))))

(defun* (backup-all -> list) ((games hash-table))
  (~>> games
       alexandria:hash-table-alist
       (mapcar 'backup-game)))

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

(defun treat-backup-as-complete (condition)
  (declare (ignore condition))
  (invoke-restart 'treat-backup-as-complete))

(defparameter *backup-game-callback* nil)

(tu:desfun backup-game ((game-name . (&key save-path save-glob)) &key count-only)
  (let ((start-time (get-internal-real-time))
        (file-count 0))
    (flet ((complete-callback ()
             (bind ((end-time (get-internal-real-time)))
               (when (and (not count-only) *backup-game-callback*)
                 (funcall *backup-game-callback*
                          game-name
                          (get-universal-time)
                          (/ (- end-time start-time)
                             internal-time-units-per-second))))))
      (restart-case
          (handler-case
              (progn
                (cl-fad:walk-directory save-path
                                       (op (incf file-count
                                                 (backup-file game-name save-path _
                                                              :count-only count-only)))
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
                (complete-callback)
                file-count)
            (error ()
              (error 'backup-game-error :game-name game-name
                                        :game-path save-path)))
        (skip-game () 1)
        (treat-backup-as-complete ()
          (complete-callback))))))

(define-condition backup-file-error (backup-error file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unable to back up file ~a for ~a"
                     (file-error-pathname condition)
                     (backup-error-game-name condition)))))

(defun skip-file (condition)
  (declare (ignore condition))
  (invoke-restart 'skip-file))

(defun treat-file-as-copied (condition)
  (declare (ignore condition))
  (invoke-restart 'treat-file-as-copied))

(defun backup-file-name (from to)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (or (file-write-date from) 0) 0)
    (format nil "~a.bak.~4,'0d_~2,'0d_~2,'0d_~2,'0d_~2,'0d_~2,'0d"
            to year month day hour min sec)))

(defparameter *backup-file-callback* nil)

(defun* (backup-file -> fixnum) ((game-name string)
                                 (save-path (or string pathname))
                                 (from (or string pathname))
                                 &key count-only)
  (restart-case
      (handler-case
          (bind ((save-path (cl-fad:pathname-as-directory save-path))
                 (backup-path (path:catdir (cl-fad:pathname-as-directory *backup-path*)
                                           (cl-fad:pathname-as-directory game-name)))
                 (relative-save-path (subseq (namestring from) (length (namestring save-path))))
                 (to (path:catfile backup-path relative-save-path))
                 (full-to (backup-file-name from to)))
            (unless (probe-file full-to)
              (unless count-only
                (ensure-directories-exist (path:dirname full-to))
                (cl-fad:copy-file from to :overwrite t)
                (cl-fad:copy-file to full-to)
                (when *backup-file-callback*
                  (funcall *backup-file-callback* from to))
                (clean-up to))
              (return-from backup-file 1))
            (return-from backup-file 0))
        (file-error (condition)
          (error condition))
        (error ()
          (error 'backup-file-error :game-name game-name
                                    :pathname from)))
    (skip-file () 1)
    (treat-file-as-copied ()
      (if *backup-file-callback*
          (funcall *backup-file-callback* from nil)
          1))))

(define-condition clean-up-error (sbu-error file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Could not clean up extra backups for ~a"
                     (file-error-pathname condition)))))

(defun skip-clean-up (condition)
  (declare (ignore condition))
  (invoke-restart 'skip-clean-up))

(defparameter *clean-up-callback* nil)

(defun* clean-up ((file-path (or string pathname)))
  (restart-case
      (handler-case
          (let ((files (directory (serapeum:string+ file-path ".bak.*_*_*_*_*_*"))))
            (when (> (length files) *backups-to-keep*)
              (let ((files-to-delete (serapeum:drop *backups-to-keep*
                                                    (sort files (lambda (f1 f2)
                                                                  (> (file-write-date f1)
                                                                     (file-write-date f2)))))))
                (mapcar #'delete-file files-to-delete)
                (when *clean-up-callback*
                  (funcall *clean-up-callback* files-to-delete)))))
        (error ()
          (error 'clean-up-error :pathname file-path)))
    (skip-clean-up () nil)))

(defun* (save-game -> list) ((games hash-table)
                             (game-name string)
                             (game-save-path (or string pathname))
                             (game-save-glob string)
                             &optional ((old-game-name (or string null)) nil))
  (remhash old-game-name games)
  (setf (gethash game-name games) `(:save-path ,game-save-path
                                    :save-glob ,game-save-glob))
  (save-games games))

(defun* (remove-game -> list) ((games hash-table) (game-name string))
  (remhash game-name games)
  (save-games games))
