(defpackage #:sbu/tests
  (:use #:cl #:5am #:mockingbird)
  (:import-from #:travv0.utils #:canonicalize-path)
  (:local-nicknames (#:tu #:travv0.utils)
                    (#:a #:alexandria))
  (:export #:run-tests #:generate-coverage #:fs-mock))

(in-package #:sbu/tests)

(def-suite sbu :in sbu/cli/tests:sbu/cli)
(in-suite sbu)

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

(test saving-and-loading
  (fad:with-open-temporary-file (f)
    (sbu:save-config sbu/tests/utils:*default-config* (pathname f))
    (is (equalp sbu/tests/utils:*default-config*
                (sbu:load-config (pathname f))))

    (let ((sbu:*games-path* (pathname f)))
      (sbu:save-games sbu/tests/utils:*default-games*)
      (is (equalp sbu/tests/utils:*default-games*
                  (sbu:load-games))))))
