;;;; Depends on Trivial Gamekit
(ql:quickload "trivial-gamekit")

;;; where to looks for game assets, relative to source directory
(gamekit:register-resource-package :keyword "assets/")
(gamekit:define-image :sprite-sheet "sprites.png")


;;;; Global Variables

;;; width and height of game window
(defvar *canvas-width* 576)
(defvar *canvas-height* 576)

;;; Position of sprite on sprites sheet starting in bottom left corner

;; sprites to draw the "map"
(defparameter *w1* (gamekit:vec2 0 224))  ; wall variant 1
(defparameter *w2* (gamekit:vec2 16 224)) ; wall variant 2
(defparameter *w3* (gamekit:vec2 32 224)) ; wall variant 3
(defparameter *f1* (gamekit:vec2 0 208))  ; floor variant 1
(defparameter *f2* (gamekit:vec2 16 208)) ; floor variant 2
(defparameter *f3* (gamekit:vec2 32 208)) ; floor variant 3
(defparameter *b1* (gamekit:vec2 0 240))  ; block variant 1
(defparameter *b2* (gamekit:vec2 16 240)) ; block variant 2

;; sprites to draw player and indicate direction  
(defparameter *player-sprite* (gamekit:vec2 0 192))        ; player facing front/up
(defparameter *player-sprite-up* (gamekit:vec2 16 192))    ; player facing front/up
(defparameter *player-sprite-down* (gamekit:vec2 0 192))   ; player facing down/bottom
(defparameter *player-sprite-right* (gamekit:vec2 32 192)) ; player facing right
(defparameter *player-sprite-left* (gamekit:vec2 48 192))  ; player facing left

;;; Player variables for location on map
;;  player location in a game grid
(defvar *player-x* 4) ; player left/right on map
(defvar *player-y* 3) ; player up/down on map

;;; Game Map/World in a 2D array
;;  nil is absence of a block, walls and blocks coresponds to sprite variables above
(setf *block-map* (make-array '(9 9)
  :initial-contents `(
  (,*w1* ,*w2* ,*w3* ,*w1* ,*f3* ,*w3* ,*w1* ,*w2* ,*w3*)
  (,*w1* ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,*w3*)
  (,*w1* ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,*w3*)
  (,*w1* ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,*w3*)
  (,*w1* ,'nil ,'nil ,'nil ,*b1* ,'nil ,'nil ,'nil ,*w3*)
  (,*w1* ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,*w3*)
  (,*w1* ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,*w3*)
  (,*w1* ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,'nil ,*w3*)
  (,*w1* ,*w2* ,*w3* ,*w1* ,*w2* ,*w3* ,*w1* ,*w2* ,*w3*)))
)

;;;; Game Logic

;;; Some initial setup, required by trivial-gamekit
(gamekit:defgame block-push () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Block Push"))

;;; Map keys for player movement
(defmethod gamekit:post-initialize ((app block-push))
  (gamekit:bind-button :escape :pressed
    (lambda () (gamekit:stop)))
  ;; Swap player sprite according to directional key pressed
  ;; Move player-x and player-y
  (gamekit:bind-button :up :pressed
    (lambda () (move-player 1 0) (setf *player-sprite* *player-sprite-up*)))
  (gamekit:bind-button :left :pressed
    (lambda () (move-player 0 -1) (setf *player-sprite* *player-sprite-left*)))
  (gamekit:bind-button :right :pressed
    (lambda () (move-player 0 1) (setf *player-sprite* *player-sprite-right*)))
  (gamekit:bind-button :down :pressed
    (lambda () (move-player -1 0) (setf *player-sprite* *player-sprite-down*))))

;;; Change player-x and player-y
;; move-player expects a 1 or 0 for each direction
(defun move-player(ud lr)
	  ;; check if a block is in the way of the players intended location
	  (if (equal *b1* (collides (+ *player-x* ud) (+ *player-y* lr)))
	      ;; push block if so
	      (push-block ud lr)
	      ;; if player will collide with anything but a block (*b1*) don't allow movement (return empty case)
	      (if (collides (+ ud *player-x*) (+ lr *player-y*))
		 ()
		 ;; else move player
                 (progn (setf *player-x* (+ ud *player-x*))
                        (setf *player-y* (+ lr *player-y*))))))

;;; Check for objects on Map/World grid given x and y
(defun collides (x y)
  (if (aref *block-map* x y)
     ;; return the object at the coords (in the 2D array0
     (aref *block-map* x y)
     ;; return nil (false) if nothing is on the map at x y
     ()))

;;; Push a block left/right up/down given direction
(defun push-block (ud lr)
  ;; Check that there is an empty space on the map one step ahead of the direction to push
  (if (collides (+ ud ud *player-x*) (+ lr lr *player-y*))
    ;; if there is something else, you can't push the block
    ()
    ;; if there is room, push the block!
    (progn 
       ;; Swap empty spot ahead of block with the block, giving the illusion of pushing
       (rotatef 
           (aref *block-map* (+ ud *player-x*) (+ lr *player-y*)) 
           (aref *block-map* (+ ud ud *player-x*) (+ lr lr *player-y*)))
       ;; Move player in into the freshly swapped empty spot
       (move-player ud lr))))

;;; Draw the floor
;; draw floor sprite at x y coord
(defun draw-floor (x y)
  (gamekit:draw-image (gamekit:vec2 (* 16 y) (* 16 x)) :sprite-sheet
                              :origin *f1*
                              :width 16
                              :height 16))

;;; Draw block to screen at x y coord if block-type is empty draw floor
(defun draw-block (block-type x y)
  (if block-type
    ;; Draw game tile from position on sprite sheet (block type)
    (gamekit:draw-image (gamekit:vec2 (* 16 y) (* 16 x)) :sprite-sheet
                            :origin block-type
                            :width 16
                            :height 16)
    ;; if block-type is empty draw the floor instead
    (draw-floor x y)))

;;; Loop through the Map (2D array) and draw blocks in corresponding positions on screen
(defun draw-map ()
    (loop 
      for x from 0 to 8 do
      (loop 
	 for y from 0 to 8 do
            (draw-block (aref *block-map* x y) x y))))

;;; Draw player to screen aligned with game grid x y coords
(defun draw-player(x y)
  (gamekit:draw-image (gamekit:vec2 (* 16 y) (* 16 x)) :sprite-sheet
                              :origin *player-sprite*
                              :width 16
                              :height 16))

;;;; Game Loop
;;; Draw all the elements together
(defmethod gamekit:draw ((app block-push))
  (gamekit:scale-canvas 4 4)
  (draw-map)
  (draw-player *player-x* *player-y*))

;;;; Start Game
(gamekit:start 'block-push)
