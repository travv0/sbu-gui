(defpackage :sbu
  (:use #:cl #:alexandria #:serapeum #:metabang-bind)
  (:export #:save-games
           #:load-games
           #:backup-all
           #:backup-game
           #:save-game
           #:remove-game))

(in-package :sbu)

(defparameter *games-path* "~/.sbugames")
(defparameter *backup-path* "~/.sbu-backups/")

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

(defun backup-all (games)
  (~>> games
       hash-table-alist
       (mapcar 'backup-game)))

(tu:desfun backup-game ((game-name . (&key save-path save-glob)))
  (cl-fad:walk-directory save-path
                         (curry 'backup-file game-name save-path)
                         :directories :depth-first
                         :test (lambda (file)
                                 (and (not (cl-fad:directory-pathname-p file))
                                      (pathname-match-p (cl-fad:pathname-as-file file)
                                                        (path:catfile
                                                         (cl-fad:pathname-as-directory save-path)
                                                         (if (string= (or save-glob "") "")
                                                             "**/*"
                                                             save-glob)))))))

(defun backup-file (game-name save-path from)
  (bind ((save-path (cl-fad:pathname-as-directory save-path))
         (backup-path (path:catdir *backup-path* (cl-fad:pathname-as-directory game-name)))
         (relative-save-path (subseq (namestring from) (length (namestring save-path))))
         (to (path:catfile backup-path relative-save-path))
         ((:values sec min hour day month year) (decode-universal-time (file-write-date from) 0))
         (full-to (format nil
                          "~a.bak.~4,'0d_~2,'0d_~2,'0d_~2,'0d_~2,'0d_~2,'0d"
                          to year month day hour min sec)))
    (unless (probe-file full-to)
      (ensure-directories-exist (path:dirname full-to))
      (cl-fad:copy-file from to :overwrite t)
      (cl-fad:copy-file to full-to)
      (format t "~a ==>~%~4t~a~%" from to))))

(defun save-game (games game-name game-save-path game-save-glob &optional old-game-name)
  (remhash old-game-name games)
  (setf (gethash game-name games) `(:save-path ,game-save-path
                                    :save-glob ,game-save-glob))
  (save-games games))

(defun remove-game (games game-name)
  (remhash game-name games)
  (save-games games))
