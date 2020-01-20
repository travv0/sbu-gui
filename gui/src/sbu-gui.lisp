(defpackage :sbu/gui
  (:use #:cl #:alexandria #:serapeum)
  (:import-from :metabang-bind #:bind)
  (:export #:start))

(in-package :sbu/gui)

(capi:define-interface window ()
  ((game-title-width :initarg :game-title-width :initform 20)
   (button-width :initarg :button-width :initform 15)
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
  (:default-initargs :title "Save Backup"
                     :internal-border 5))

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
   (file-count :initarg :file-count :initform (error "file count is required"))
   (width :initarg :width :initform 400))
  (:panes
   (progress-bar capi:progress-bar
                 :start 0
                 :end file-count
                 :min-width width
                 :max-width width)
   (last-file capi:title-pane :max-width width))
  (:layouts
   (main-layout capi:column-layout '(last-file progress-bar)))
  (:default-initargs :title "Backup Progress"))

(defparameter *completed-message-format* "Finished backing up ~a in ~fs ~
on ~a, ~a ~d ~d at ~2,'0d:~2,'0d:~2,'0d (GMT~@d)~@[

The following warnings occurred:~%~{~a~%~}~]")

(defun display-completed-message (game-name time-completed seconds-passed warnings)
  (bind (((:values second minute hour date month year day-of-week _ tz)
          (decode-universal-time time-completed)))
    (capi:display-message *completed-message-format*
                          game-name
                          seconds-passed
                          (nth day-of-week *day-names*)
                          (nth month *month-names*)
                          date
                          year
                          hour
                          minute
                          second
                          (- tz)
                          warnings)))

(defparameter *day-names*
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter *month-names*
  '("Jan" "Feb" "Mar" "Apr" "May"
    "Jun" "Jul" "Aug" "Sep" "Oct"
    "Nov" "Dec"))

(eval-when (:compile-toplevel)
  (defun push-warning (restart-function warnings)
    `(lambda (condition)
       (push (format nil "~a" condition) ,warnings)
       (funcall ',restart-function condition))))

(defmacro push-backup-game-warning (warnings)
  (push-warning 'sbu:treat-backup-as-complete warnings))

(defmacro push-backup-file-warning (warnings)
  (push-warning 'sbu:treat-file-as-copied warnings))

(defmacro push-clean-up-warning (warnings)
  (push-warning 'sbu:skip-clean-up warnings))

(defun backup (interface)
  (bind (warnings
         ((:slots games game-list) interface)
         (selected-id (capi:choice-selection game-list))
         (selected-game-name (capi:get-collection-item game-list selected-id))
         (game-data (assoc selected-game-name (hash-table-alist games)))
         (file-count (or (ignore-errors (sbu:backup-game game-data :count-only t))
                         0))
         (progress-window (capi:display (make-instance 'progress-window
                                                       :game-name selected-game-name
                                                       :file-count file-count))))

    (with-slots (progress-bar last-file) progress-window
      (flet ((backup-file-callback (from to)
               (declare (ignore to))
               (setf (capi:title-pane-text last-file) (namestring from))
               (incf (capi:range-slug-start progress-bar)))

             (backup-game-callback (game-name finish-time seconds-passed)
               (display-completed-message game-name finish-time seconds-passed warnings)
               (capi:destroy progress-window)))

        (handler-bind ((sbu:backup-game-error (lambda (c)
                                                (capi:display-message "Error: ~a" c)
                                                (capi:destroy progress-window)
                                                (abort c)))
                       (sbu:backup-file-error (push-backup-file-warning warnings))
                       (sbu:clean-up-error (push-clean-up-warning warnings)))
          (let ((sbu:*backup-file-callback* #'backup-file-callback)
                (sbu:*backup-game-callback* #'backup-game-callback))
            (sbu:backup-game game-data)))))))

(capi:define-interface multiple-progress-window ()
  ((game-count :initarg :game-count :initform (error "game count is required"))
   (file-count :initarg :file-count :initform (error "file count is required"))
   (width :initarg :width :initform 400))
  (:panes
   (file-progress-bar capi:progress-bar
                      :start 0
                      :end file-count
                      :min-width width
                      :max-width width)
   (game-progress-bar capi:progress-bar
                      :start 0
                      :end game-count
                      :min-width width
                      :max-width width)
   (last-file capi:title-pane :max-width width)
   (last-game capi:title-pane :max-width width))
  (:layouts
   (main-layout capi:column-layout '(last-game
                                     game-progress-bar
                                     last-file
                                     file-progress-bar)))
  (:default-initargs :title "Backup Progress"))

(defmacro zero-if-error (n)
  `(or (ignore-errors ,n) 0))

(defun number-of-files-to-backup (game)
  (zero-if-error (sbu:backup-game game :count-only t)))

(defun backup-all (interface)
  (when-let ((games-alist (hash-table-alist (games interface))))
    (let* ((total-seconds 0)
           warnings
           (multi-progress-window (capi:display
                                   (make-instance 'multiple-progress-window
                                                  :game-count (length games-alist)
                                                  :file-count (number-of-files-to-backup
                                                               (car games-alist))))))
      (with-slots (file-progress-bar last-file game-progress-bar last-game)
          multi-progress-window
        (flet ((backup-file-callback (from to)
                 (declare (ignore to))
                 (setf (capi:title-pane-text last-file) (namestring from))
                 (incf (capi:range-slug-start file-progress-bar)))

               (backup-game-callback (game-name time-complete seconds-passed)
                 (incf (capi:range-slug-start game-progress-bar))
                 (incf total-seconds seconds-passed)
                 (setf (capi:title-pane-text last-game) game-name)
                 (cond ((>= (capi:range-slug-start game-progress-bar)
                            (capi:range-end game-progress-bar))
                        (display-completed-message "all" time-complete seconds-passed warnings)
                        (capi:destroy multi-progress-window))
                       (t (setf (capi:range-end file-progress-bar)
                                (number-of-files-to-backup (nth (capi:range-slug-start game-progress-bar)
                                                                games-alist))
                                (capi:range-slug-start file-progress-bar) 0
                                (capi:title-pane-text last-file) "")))))

          (handler-bind ((sbu:backup-game-error (push-backup-game-warning warnings))
                         (sbu:backup-file-error (push-backup-file-warning warnings))
                         (sbu:clean-up-error (push-clean-up-warning warnings)))
            (let ((sbu:*backup-file-callback* #'backup-file-callback)
                  (sbu:*backup-game-callback* #'backup-game-callback))
              (sbu:backup-all (games interface)))))))))
