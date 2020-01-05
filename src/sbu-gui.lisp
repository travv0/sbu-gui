(defpackage :sbu-gui
  (:use #:travv0.prelude #:capi))

(in-package :sbu-gui)

(define-interface window ()
  ((game-title-width :initarg :game-title-width :initform 17)
   (button-width :initarg :button-width :initform 12))
  (:panes
   (list-buttons push-button-panel
                 :items '(:backup :remove)
                 :default-button :backup
                 :print-function 'string-capitalize
                 :layout-class 'column-layout
                 :layout-args `(:visible-min-width (:character ,button-width)))
   (game-list list-panel
              :items '("New...")
              :visible-min-width '(:character 40)
              :visible-min-height '(:character 10))
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
   (save-button push-button :data "Add" :visible-min-width `(:character ,button-width)))
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

(defun start ()
  (display (make-instance 'window)))
