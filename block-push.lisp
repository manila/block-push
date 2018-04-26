(ql:quickload "trivial-gamekit")

(gamekit:defgame block-push () ())
(gamekit:register-resource-package :keyword "assets/")
(gamekit:define-image :sprite-sheet "sprites.png")

(defvar *canvas-width* 576)
(defvar *canvas-height* 576)

(gamekit:defgame block-push () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Block Push"))

(defmethod gamekit:draw ((app block-push))
  (gamekit:draw-image (gamekit:vec2 0 0) :sprite-sheet
		      :origin (gamekit:vec2 0 960)
		      :width 64
		      :height 64))


(gamekit:start 'block-push)

