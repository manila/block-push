(ql:quickload "trivial-gamekit")

(gamekit:register-resource-package :keyword "assets/")
(gamekit:define-image :sprite-sheet "sprites.png")

(defvar *canvas-width* 576)
(defvar *canvas-height* 576)
(defparameter *w1* (gamekit:vec2 0 224))
(defparameter *w2* (gamekit:vec2 16 224))
(defparameter *w3* (gamekit:vec2 32 224))
(defparameter *f1* (gamekit:vec2 0 208))
(defparameter *f2* (gamekit:vec2 16 208))
(defparameter *b1* (gamekit:vec2 0 240))
(defparameter *b2* (gamekit:vec2 16 240))
(defparameter *player-sprite* (gamekit:vec2 0 192))
(defvar *player-x* 4)
(defvar *player-y* 3)
(setf *block-map* (make-array '(9 9)
  :initial-contents `(
  (,*w1* ,*w2* ,*w3* ,*w1* ,'nil ,*w3* ,*w1* ,*w2* ,*w3*)
  (,*w1* ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,*w3*)
  (,*w1* ,'nil ,*b1* ,*b1* ,*b1* ,*b1* ,*b1* ,'nil ,*w3*)
  (,*w1* ,'nil ,*b1* ,'nil ,'nil ,'nil ,*b1* ,'nil ,*w3*)
  (,*w1* ,'nil ,*b1* ,'nil ,*b1* ,'nil ,*b1* ,'nil ,*w3*)
  (,*w1* ,'nil ,*b1* ,'nil ,'nil ,'nil ,*b1* ,'nil ,*w3*)
  (,*w1* ,'nil ,*b1* ,*b1* ,*b1* ,*b1* ,*b1* ,'nil ,*w3*)
  (,*w1* ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,*w3*)
  (,*w1* ,*w2* ,*w3* ,*w1* ,*w2* ,*w3* ,*w1* ,*w2* ,*w3*)))
)

(gamekit:defgame block-push () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Block Push"))

(defmethod gamekit:post-initialize ((app block-push))
  (gamekit:bind-button :escape :pressed
    (lambda () (gamekit:stop)))
  (gamekit:bind-button :up :pressed
    (lambda () (move-player 1 0)))
  (gamekit:bind-button :left :pressed
    (lambda () (move-player 0 -1)))
  (gamekit:bind-button :right :pressed
    (lambda () (move-player 0 1)))
  (gamekit:bind-button :down :pressed
    (lambda () (move-player -1 0))))

(defun move-player(ud lr)
	  (if (equal *b1* (collides (+ *player-x* ud) (+ *player-y* lr)))
	      (push-block ud lr)
	      (if (collides (+ ud *player-x*) (+ lr *player-y*))
		 ()
                 (progn (setf *player-x* (+ ud *player-x*))
                        (setf *player-y* (+ lr *player-y*))))))

(defun collides (x y)
  (if (aref *block-map* x y)
     (aref *block-map* x y)
     ()))

(defun push-block (ud lr)
  (if (collides (+ ud ud *player-x*) (+ lr lr *player-y*))
    ()
    (progn (rotatef (aref *block-map* (+ ud *player-x*) (+ lr *player-y*)) (aref *block-map* (+ ud ud *player-x*) (+ lr lr *player-y*)))
           (move-player ud lr))))

(defun draw-floor (x y)
  (gamekit:draw-image (gamekit:vec2 (* 16 y) (* 16 x)) :sprite-sheet
                              :origin *f1*
                              :width 16
                              :height 16))


(defun draw-block (block-type x y)
  (if block-type
    (gamekit:draw-image (gamekit:vec2 (* 16 y) (* 16 x)) :sprite-sheet
                            :origin block-type
                            :width 16
                            :height 16)
    (draw-floor x y)))

(defun draw-map ()
    (loop 
      for x from 0 to 8 do
      (loop 
	 for y from 0 to 8 do
            (draw-block (aref *block-map* x y) x y))))

(defun draw-player(x y)
  (gamekit:draw-image (gamekit:vec2 (* 16 y) (* 16 x)) :sprite-sheet
                              :origin *player-sprite*
                              :width 16
                              :height 16))

(defmethod gamekit:draw ((app block-push))
  (gamekit:scale-canvas 4 4)
  (draw-map)
  (draw-player *player-x* *player-y*))

(gamekit:start 'block-push)
