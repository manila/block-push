(ql:quickload "trivial-gamekit")

(gamekit:register-resource-package :keyword "assets/")
(gamekit:define-image :sprite-sheet "sprites.png")

(defvar *canvas-width* 576)
(defvar *canvas-height* 576)
(defparameter *w1* (gamekit:vec2 0 896))
(defparameter *w2* (gamekit:vec2 64 896))
(defparameter *w3* (gamekit:vec2 128 896))
(defparameter *f1* (gamekit:vec2 0 832))
(defparameter *f2* (gamekit:vec2 64 832))
(defparameter *b1* (gamekit:vec2 0 960))
(defparameter *b2* (gamekit:vec2 64 960))
(defparameter *player-sprite* (gamekit:vec2 0 768))
(defvar *player-x* 2)
(defvar *player-y* 2)
(defvar *block-map* `(
  ,(list *w1* *w2* *w3* *w1* 'nil *w3* *w1* *w2* *w3*)
  ,(list *w1* 'nil 'nil 'nil 'nil 'nil 'nil 'nil *w3*)
  ,(list *w1* 'nil *b1* *b2* *b1* *b1* *b1* 'nil *w3*)
  ,(list *w1* 'nil *b1* 'nil 'nil 'nil *b1* 'nil *w3*)
  ,(list *w1* 'nil *b1* 'nil *b1* 'nil *b1* 'nil *w3*)
  ,(list *w1* 'nil *b1* 'nil 'nil 'nil *f1* 'nil *w3*)
  ,(list *w1* 'nil *b1* *b1* *b2* *b1* *b2* 'nil *w3*)
  ,(list *w1* 'nil 'nil 'nil 'nil 'nil 'nil 'nil *w3*)
  ,(list *w1* *w2* *w3* *w1* *w2* *w3* *w1* *w2* *w3*)))

(gamekit:defgame block-push () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Block Push"))

(gamekit:bind-button :mouse-left :released
  (lambda () (setf *player-x* nil)))

(defun draw-floor (x y)
  (gamekit:draw-image (gamekit:vec2 (* 64 y) (* 64 x)) :sprite-sheet
                              :origin *f1*
                              :width 64
                              :height 64))


(defun draw-block (block-type x y)
  (if block-type
    (gamekit:draw-image (gamekit:vec2 (* 64 y) (* 64 x)) :sprite-sheet
                            :origin block-type
                            :width 64
                            :height 64)
    (draw-floor x y)))

(defun draw-map ()
    (loop 
      for x from 0 to 10
      for row in *block-map* do
      (loop 
	 for y from 0 to 10
         for block-type in row do 
            (draw-block block-type x y))))

(defun draw-player(x y)
  (gamekit:draw-image (gamekit:vec2 (* 64 y) (* 64 x)) :sprite-sheet
                              :origin *player-sprite*
                              :width 64
                              :height 64))

(defmethod gamekit:draw ((app block-push))
  (draw-map)
  (draw-player *player-x* *player-y*))

(gamekit:start 'block-push)
