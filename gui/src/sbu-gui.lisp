(defpackage :sbu/gui
  (:use #:cl #:alexandria #:serapeum)
  (:import-from :metabang-bind #:bind)
  (:export #:start))

(in-package :sbu/gui)

(capi:define-interface window ()
  ((game-title-width :initarg :game-title-width :initform 17)
   (button-width :initarg :button-width :initform 12)
   (games :initarg :games
          :initform (make-hash-table :test 'equal)
          :reader games))
  (:panes
   (list-buttons capi:push-button-panel
                 :items (list backup-all-button backup-button remove-button)
                 :default-button backup-all-button
                 :layout-class 'capi:column-layout)
   (backup-all-button capi:push-button
                      :text "Backup All"
                      :visible-min-width `(:character ,button-width)
                      :callback-type :interface
                      :callback 'backup-all)
   (backup-button capi:push-button
                  :text "Backup"
                  :visible-min-width `(:character ,button-width)
                  :enabled nil
                  :callback-type :interface
                  :callback 'backup)
   (remove-button capi:push-button
                  :text "Remove"
                  :visible-min-width `(:character ,button-width)
                  :enabled nil
                  :callback-type :interface
                  :callback 'remove-game)
   (game-list capi:list-panel
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
   (game-name capi:text-input-pane
              :title "Game Name"
              :title-args `(:visible-min-width (:character ,game-title-width))
              :text-change-callback 'edit-game-callback)
   (game-save-path capi:text-input-pane
                   :title "Game Save Path"
                   :title-args `(:visible-min-width (:character ,game-title-width))
                   :file-completion t
                   :directories-only t
                   :buttons '(:ok nil
                              :browse-file (:directory t))
                   :text-change-callback 'edit-game-callback)
   (game-save-glob capi:text-input-pane
                   :title "Game Save Glob"
                   :title-args `(:visible-min-width (:character ,game-title-width))
                   :text-change-callback 'edit-game-callback)
   (save-button capi:push-button :data "Save"
                                 :visible-min-width `(:character ,button-width)
                                 :callback-type :interface
                                 :callback 'save-game
                                 :enabled nil))
  (:layouts
   (main-layout capi:column-layout '(games-layout game-edit-layout))
   (games-layout capi:row-layout '(game-list list-buttons))
   (game-edit-layout capi:column-layout
                     '(game-name
                       game-save-path
                       game-save-glob
                       save-button)
                     :adjust :right))
  (:default-initargs :title "Save Backup"))

(defun edit-game-callback (text pane interface d)
  (declare (ignore text pane d))
  (with-slots (backup-all-button backup-button remove-button save-button) interface
    (setf (capi:simple-pane-enabled backup-all-button) nil
          (capi:simple-pane-enabled backup-button) nil
          (capi:simple-pane-enabled remove-button) nil
          (capi:simple-pane-enabled save-button) t)))

(defun select-game (data interface)
  (bind (((:slots games backup-all-button backup-button remove-button save-button
                  game-name game-save-path game-save-glob)
          interface)
         ((:accessors (game-name capi:text-input-pane-text)) game-name)
         ((:accessors (game-save-path capi:text-input-pane-text)) game-save-path)
         ((:accessors (game-save-glob capi:text-input-pane-text)) game-save-glob)
         ((:plist save-path save-glob) (gethash data games)))
    (cond ((string= data "New...") (select-new interface))
          (t (setf (capi:simple-pane-enabled backup-all-button) t
                   (capi:simple-pane-enabled backup-button) t
                   (capi:simple-pane-enabled remove-button) t
                   (capi:simple-pane-enabled save-button) nil
                   game-name data
                   game-save-path save-path
                   game-save-glob save-glob)))))

(defun select-new (interface)
  (bind (((:slots backup-all-button backup-button remove-button save-button
                  game-name game-save-path game-save-glob)
          interface)
         ((:accessors (game-name capi:text-input-pane-text)) game-name)
         ((:accessors (game-save-path capi:text-input-pane-text)) game-save-path)
         ((:accessors (game-save-glob capi:text-input-pane-text)) game-save-glob))
    (setf (capi:simple-pane-enabled backup-all-button) t
          (capi:simple-pane-enabled backup-button) nil
          (capi:simple-pane-enabled remove-button) nil
          (capi:simple-pane-enabled save-button) nil
          game-name ""
          game-save-path ""
          game-save-glob "")))

(defun reselect-game (data interface)
  (declare (ignore data))
  (with-slots (game-list) interface
    (setf (capi:choice-selection game-list) 0)
    (select-new interface)))

(defun save-game (interface)
  (bind (((:slots games game-list game-name game-save-path game-save-glob) interface)
         ((:accessors (game-name capi:text-input-pane-text)) game-name)
         ((:accessors (game-save-path capi:text-input-pane-text)) game-save-path)
         ((:accessors (game-save-glob capi:text-input-pane-text)) game-save-glob)
         (selected-id (capi:choice-selection game-list))
         (selected-game-name (capi:get-collection-item game-list selected-id))
         ((:accessors (game-list capi:collection-items)) game-list))
    (sbu:save-game games game-name game-save-path game-save-glob selected-game-name)
    (setf game-list games)
    (select-new interface)))

(defun remove-game (interface)
  (bind (((:slots games game-list) interface)
         (selected-id (capi:choice-selection game-list))
         (selected-game-name (capi:get-collection-item game-list selected-id))
         ((:accessors (game-list capi:collection-items)) game-list))
    (when (capi:confirm-yes-or-no "Are you sure you would like to remove ~a from the list?"
                                  selected-game-name)
      (sbu:remove-game games selected-game-name)
      (setf game-list games)
      (select-new interface))))

(defun start ()
  (capi:display (make-instance 'window :games (sbu:load-games))))

(capi:define-interface progress-window ()
  ((game-name :initarg :game-name :initform (error "game name is required"))
   (file-count :initarg :file-count :initform (error "file count is required")))
  (:panes
   (progress-bar capi:progress-bar
                 :start 0
                 :end file-count)
   (last-file capi:title-pane))
  (:layouts
   (main-layout capi:column-layout '(last-file progress-bar))))

(defun backup (interface)
  (bind (((:slots games game-list) interface)
         (selected-id (capi:choice-selection game-list))
         (selected-game-name (capi:get-collection-item game-list selected-id))
         (game-data (assoc selected-game-name (hash-table-alist games)))
         (file-count (sbu:backup-game game-data :count-only t))
         (progress-window (capi:display (make-instance 'progress-window
                                                       :game-name selected-game-name
                                                       :file-count file-count))))
    (with-slots (progress-bar last-file) progress-window
      (handler-bind ((sbu:file-copied (lambda (condition)
                                        (setf (capi:title-pane-text last-file)
                                              (namestring (sbu:file-copied-from condition)))
                                        (incf (capi:range-slug-start progress-bar))))
                     (sbu:backup-complete (lambda (condition)
                                            (capi:display-message "~a" condition)
                                            (capi:destroy progress-window))))
        (sbu:backup-game game-data)))))

(capi:define-interface multiple-progress-window ()
  ((game-count :initarg :game-count :initform (error "game count is required"))
   (file-count :initarg :file-count :initform (error "file count is required")))
  (:panes
   (file-progress-bar capi:progress-bar
                      :start 0
                      :end file-count)
   (game-progress-bar capi:progress-bar
                      :start 0
                      :end game-count)
   (last-file capi:title-pane)
   (last-game capi:title-pane))
  (:layouts
   (main-layout capi:column-layout '(last-game
                                     game-progress-bar
                                     last-file
                                     file-progress-bar))))

(defun backup-all (interface)
  (let* ((total-seconds 0)
         (games-alist (hash-table-alist (games interface)))
         (multi-progress-window (capi:display
                                 (make-instance 'multiple-progress-window
                                                :game-count (length games-alist)
                                                :file-count (sbu:backup-game (car games-alist)
                                                                             :count-only t)))))
    (with-slots (file-progress-bar last-file game-progress-bar last-game)
        multi-progress-window
      (handler-bind ((sbu:file-copied (lambda (condition)
                                        (setf (capi:title-pane-text last-file)
                                              (namestring (sbu:file-copied-from condition)))
                                        (incf (capi:range-slug-start file-progress-bar))))
                     (sbu:backup-complete (lambda (condition)
                                            (incf (capi:range-slug-start game-progress-bar))
                                            (incf total-seconds
                                                  (sbu:backup-complete-seconds-passed condition))
                                            (cond ((>= (capi:range-slug-start game-progress-bar)
                                                       (capi:range-end game-progress-bar))
                                                   (capi:display-message
                                                    "~a"
                                                    (make-condition 'sbu:backup-complete
                                                                    :game-name "all games"
                                                                    :finish-time (sbu:backup-complete-finish-time
                                                                                  condition)
                                                                    :seconds-passed total-seconds))
                                                   (capi:destroy multi-progress-window))
                                                  (t (setf (capi:title-pane-text last-game)
                                                           (sbu:backup-complete-game-name condition)
                                                           (capi:range-end file-progress-bar)
                                                           (sbu:backup-game (nth (capi:range-slug-start
                                                                                  game-progress-bar)
                                                                                 games-alist)
                                                                            :count-only t)
                                                           (capi:range-slug-start file-progress-bar) 0
                                                           (capi:title-pane-text last-file) ""))))))
        (sbu:backup-all (games interface))))))
