(defpackage #:sbu/tests/utils
  (:use #:cl #:5am #:mockingbird)
  (:local-nicknames (#:a #:alexandria)
                    (#:tu #:travv0.utils))
  (:export #:fs-mock #:*default-games* #:*default-config*))

(in-package #:sbu/tests/utils)

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

(def-fixture fs-mock ()
  (with-dynamic-stubs ((sbu:save-games #'save-games-mock)
                       (sbu:load-games #'load-games-mock)
                       (sbu:save-config #'save-config-mock)
                       (sbu:load-config #'load-config-mock))
    (let (*games-storage* *config-storage*)
      (&body))))
