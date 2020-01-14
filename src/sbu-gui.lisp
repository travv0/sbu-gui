(defpackage :sbu-gui
  (:use #:travv0.prelude #:capi))

(in-package :sbu-gui)

(define-interface window ()
  ((game-title-width :initarg :game-title-width :initform 17)
   (button-width :initarg :button-width :initform 12)
   (games :initarg :games
          :initform (make-hash-table :test 'equal)
          :reader games))
  (:panes
   (list-buttons push-button-panel
                 :items '(:|backup all| :backup :remove)
                 :default-button :|backup all|
                                 :print-function 'string-capitalize
                                 :layout-class 'column-layout
                                 :layout-args `(:visible-min-width (:character ,button-width))
                                 :callbacks (list 'backup-all 'backup 'remove-game))
   (game-list list-panel
              :items games
              :items-map-function (lambda (ht f cr)
                                    (funcall (if cr #'mapcar #'mapc)
                                             f
                                             (sort (hash-table-keys ht) #'string-lessp)))
              :items-count-function (compose (curry #'+ 1) #'hash-table-count)
              :items-get-function (lambda (ht index)
                                    (if (= 0 index)
                                        "New..."
                                        (nth (1- index)
                                             (sort (hash-table-keys ht) #'string-lessp))))
              :test-function 'equal
              :visible-min-width '(:character 40)
              :visible-min-height '(:character 10)
              :selection-callback 'select-game
              :retract-callback 'reselect-game)
   (game-name text-input-pane
              :title "Game Name"
              :title-args `(:visible-min-width (:character ,game-title-width)))
   (game-save-path text-input-pane
                   :title "Game Save Path"
                   :title-args `(:visible-min-width (:character ,game-title-width))
                   :file-completion t
                   :directories-only t
                   :buttons '(:ok nil
                              :browse-file (:directory t)))
   (game-save-glob text-input-pane
                   :title "Game Save Glob"
                   :title-args `(:visible-min-width (:character ,game-title-width)))
   (save-button push-button :data "Save"
                            :visible-min-width `(:character ,button-width)
                            :callback-type :interface
                            :callback 'save-game))
  (:layouts
   (main-layout column-layout '(games-layout game-edit-layout))
   (games-layout row-layout '(game-list list-buttons))
   (game-edit-layout column-layout
                     '(game-name
                       game-save-path
                       game-save-glob
                       save-button)
                     :adjust :right))
  (:default-initargs :title "Save Backup"))

(defun select-game (data interface)
  (bind (((:slots games game-name game-save-path game-save-glob) interface)
         ((:accessors (game-name text-input-pane-text)) game-name)
         ((:accessors (game-save-path text-input-pane-text)) game-save-path)
         ((:accessors (game-save-glob text-input-pane-text)) game-save-glob)
         ((:plist save-path save-glob) (gethash data games)))
    (cond ((string= data "New...") (setf game-name ""
                                         game-save-path ""
                                         game-save-glob ""))
          (t (setf game-name data
                   game-save-path save-path
                   game-save-glob save-glob)))))

(desfun reselect-game (_data interface)
  (bind (((:slots game-name game-save-path game-save-glob game-list) interface)
         ((:accessors (game-name text-input-pane-text)) game-name)
         ((:accessors (game-save-path text-input-pane-text)) game-save-path)
         ((:accessors (game-save-glob text-input-pane-text)) game-save-glob))
    (setf (choice-selection game-list) 0)
    (setf game-name ""
          game-save-path ""
          game-save-glob "")))

(defun save-game (interface)
  (bind (((:slots games game-list game-name game-save-path game-save-glob) interface)
         ((:accessors (game-name text-input-pane-text)) game-name)
         ((:accessors (game-save-path text-input-pane-text)) game-save-path)
         ((:accessors (game-save-glob text-input-pane-text)) game-save-glob)
         (selected-id (choice-selection game-list))
         (selected-game-name (get-collection-item game-list selected-id))
         ((:accessors (game-list collection-items)) game-list))
    (remhash selected-game-name games)
    (setf (gethash game-name games) `(:save-path ,game-save-path
                                      :save-glob ,game-save-glob)
          game-list games
          game-name ""
          game-save-path ""
          game-save-glob "")
    (save-games games)))

(desfun remove-game (_data interface)
  (bind (((:slots games game-list game-name game-save-path game-save-glob) interface)
         ((:accessors (game-name text-input-pane-text)) game-name)
         ((:accessors (game-save-path text-input-pane-text)) game-save-path)
         ((:accessors (game-save-glob text-input-pane-text)) game-save-glob)
         (selected-id (choice-selection game-list))
         (selected-game-name (get-collection-item game-list selected-id))
         ((:accessors (game-list collection-items)) game-list))
    (remhash selected-game-name games)
    (setf game-list games
          game-name ""
          game-save-path ""
          game-save-glob "")
    (save-games games)))

(defparameter *games-path* "~/.sbugames")

(defun save-games (games)
  (with-open-file (out *games-path*
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (pprint (hash-table-alist games) out))))

(defun load-games ()
  (if (probe-file *games-path*)
      (with-open-file (in *games-path*)
        (with-standard-io-syntax
          (alist-hash-table (read in) :test 'equal)))
      (make-hash-table :test 'equal)))

(defun start ()
  (display (make-instance 'window :games (load-games))))

(defparameter *backup-path* "~/.sbu-backups/")

(desfun backup (_data interface)
  (bind (((:slots games game-list) interface)
         (selected-id (choice-selection game-list))
         (selected-game-name (get-collection-item game-list selected-id)))
    (backup-game (assoc selected-game-name (hash-table-alist games)))))

(desfun backup-all (_data interface)
  (->> (games interface)
       hash-table-alist
       (mapcar 'backup-game)))

(desfun backup-game ((game-name . (&key save-path save-glob)))
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
  (bind ((backup-path (path:catdir *backup-path* (cl-fad:pathname-as-directory game-name)))
         (relative-save-path (subseq (namestring from) (length (namestring save-path))))
         (to (path:catfile backup-path relative-save-path)))
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time (file-write-date from) 0)
      (bind ((full-to (format nil
                              "~a.bak.~4,'0d_~2,'0d_~2,'0d_~2,'0d_~2,'0d_~2,'0d"
                              to year month day hour min sec)))
        (unless (probe-file full-to)
          (ensure-directories-exist (path:dirname full-to))
          (cl-fad:copy-file from full-to))))))
