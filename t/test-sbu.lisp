(defpackage #:sbu/tests
  (:use #:cl #:5am #:mockingbird)
  (:import-from #:travv0.utils #:canonicalize-path)
  (:local-nicknames (#:tu #:travv0.utils)
                    (#:a #:alexandria))
  (:export #:run-tests #:generate-coverage #:fs-mock))

(in-package #:sbu/tests)

(def-suite sbu)
(in-suite sbu)

(defvar *config-storage*)
(defvar *games-storage*)

(defun run-tests ()
  (run! 'sbu))

(defun generate-coverage (directory)
  (sb-cover:clear-coverage)
  (declaim (optimize sb-cover:store-coverage-data))
  (asdf:oos 'asdf:load-op :sbu :force t)
  (run-tests)
  (sb-cover:report (fad:pathname-as-directory directory))
  (declaim (optimize (sb-cover:store-coverage-data 0)))
  (sb-cover:clear-coverage))

(defparameter *default-config*
  (serapeum:dict
   :backups-to-keep 10
   :backup-frequency 13
   :backup-path (canonicalize-path "~/.sbu-backups/")))

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

(test saving-and-loading
  (fad:with-open-temporary-file (f)
    (sbu:save-config *default-config* (pathname f))
    (is (equalp *default-config*
                (sbu:load-config (pathname f))))

    (let ((sbu:*games-path* (pathname f)))
      (sbu:save-games *default-games*)
      (is (equalp *default-games* (sbu:load-games))))))
