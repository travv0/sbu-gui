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
                 :items '(:backup :remove)
                 :default-button :backup
                 :print-function 'string-capitalize
                 :layout-class 'column-layout
                 :layout-args `(:visible-min-width (:character ,button-width))
                 :callbacks (list nil 'remove-game))
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
  (bind (((:slots games game-list game-name game-save-path game-save-glob)
          interface)
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

(defun reselect-game (data interface)
  (bind (((:slots games game-list game-name game-save-path game-save-glob)
          interface)
         ((:accessors (game-name text-input-pane-text)) game-name)
         ((:accessors (game-save-path text-input-pane-text)) game-save-path)
         ((:accessors (game-save-glob text-input-pane-text)) game-save-glob)
         ((:slots game-list) interface))
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
          game-save-glob "")))

(defun remove-game (data interface)
  (declare (ignore data))
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
          game-save-glob "")))

(defun start ()
  (display (make-instance 'window)))
