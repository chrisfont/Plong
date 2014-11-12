;;;; plong.lisp

(in-package #:plong)

;;; "plong" goes here. Hacks and glory await!

(defvar *debug* nil)

(defvar *victory* "")
(defvar *game-over* nil)

(defvar *width* 600)
(defvar *height* 400)

(defvar *player-height* 40)
(defvar *player-width*  2)

(defvar *ball-width* 2)
(defvar *ball-height* 2)

(defvar *fps*    0)
(defvar *frames* 0)
(defvar *time*   0)

(defstruct player
  (score 0 :type integer)
  (x-pos 0 :type integer)
  (y-pos 0 :type integer))

(defstruct ball
  (x-pos 0.0 :type float)
  (y-pos 0.0 :type float)
  (x-vel 0.0 :type float)
  (y-vel 0.0 :type float))

(defvar *max-score* 10)

(defvar *player-one* (make-player :x-pos (+ -50 (/ *player-width* 2))))
(defvar *player-two* (make-player :x-pos (- 50  (/ *player-width* 2))))

(defvar *ball* (make-ball :x-vel 1.0 :y-vel 0.0))

(defun draw-ball (ball)
  (let ((x-start (- (ball-x-pos ball)
                    (/ *ball-width* 2)))
        (x-end   (+ (ball-x-pos ball)
                    (/ *ball-width* 2)))
        (y-start (- (ball-y-pos ball)
                    (/ *ball-height* 2)))
        (y-end   (+ (ball-y-pos ball)
                    (/ *ball-height* 2))))
    (gl:rect x-start y-start x-end y-end)))

(defun draw-player (player)
  (let ((x-start (- (player-x-pos player)
                    (/ *player-width* 2)))
        (x-end   (+ (player-x-pos player)
                    (/ *player-width* 2)))
        (y-start (- (player-y-pos player)
                    (/ *player-height* 2)))
        (y-end   (+ (player-y-pos player)
                    (/ *player-height* 2))))
    (gl:rect x-start y-start x-end y-end)))

;; TODO: Fix error where going off board locks your pad
(defun player-up (player)
  (let ((y-top (+ (player-y-pos player) (/ *player-height* 2))))
    (when (> 50 y-top)
      (setf (player-y-pos player) (+ (player-y-pos player) 2)))))

(defun player-down (player)
  (let ((y-bot (- (player-y-pos player) (/ *player-height* 2))))
    (when (< -50 y-bot)
      (setf (player-y-pos player) (- (player-y-pos player) 2)))))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (or (eq action :press) (eq action :repeat))
    (case key
      (:escape (glfw:set-window-should-close))
      (:up     (player-up   *player-two*))
      (:down   (player-down *player-two*))
      (:w      (player-up   *player-one*))
      (:s      (player-down *player-one*))
      (:q  (setf *debug* (not *debug*)))
      (:enter  (when *game-over*
                 (setf *game-over* (not *game-over*))
                 (init))))))

(glfw:def-window-size-callback window-size-callback (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun reset-ball (ball)
  (setf (ball-x-pos ball) 0.0)
  (setf (ball-y-pos ball) 0.0)
  (setf (ball-x-vel ball)
        (if (< 0 (ball-x-vel ball))
            (- 0.5)
            0.5))
  (setf (ball-y-vel ball)
        (if (< 0 (ball-y-vel ball))
            (- 0.5)
            0.5)))

(defun update ()
  (when (>= (player-score *player-one*) *max-score*)
    (setf *victory* "Player One Wins!~%Press Enter to Restart~%Press ESC to Exit")
    (setf *game-over* t)
    (reset-ball *ball*)
    (setf (ball-x-vel *ball*) 0.0)
    (setf (ball-y-vel *ball*) 0.0))
  (when (>= (player-score *player-two*) *max-score*)
    (setf *victory* "Player One Wins!~%Press Enter to Restart~%Press ESC to Exit")
    (setf *game-over* t)
    (reset-ball *ball*)
    (setf (ball-x-vel *ball*) 0.0)
    (setf (ball-y-vel *ball*) 0.0)))

(defun player-collision-p (ball player)
  (let ((y-top   (- (player-y-pos player) (/ *player-height* 2)))
        (y-bot   (+ (player-y-pos player) (/ *player-height* 2)))
        (x-left  (- (player-x-pos player) (/ *player-width* 2)))
        (x-right (+ (player-x-pos player) (/ *player-width* 2)))
        (y       (ball-y-pos ball))
        (x       (ball-x-pos ball)))
    (and (and (<= y-top   y)
              (>= y-bot   y))
         (and (<= x-left  x)
              (>= x-right x)))))

(defun update-ball (ball)
  ;; Update ball position
  (setf (ball-y-pos ball)
        (+ (ball-y-pos ball) (ball-y-vel ball)))
  (setf (ball-x-pos ball)
        (+ (ball-x-pos ball) (ball-x-vel ball)))

  ;; Detect if we collide with wall
  (when (and (> -50 (ball-y-pos ball))
             (> 0 (ball-y-vel ball)))
    (setf (ball-y-vel ball) (- (ball-y-vel ball))))
  (when (and (< 50 (ball-y-pos ball))
             (< 0 (ball-y-vel ball)))
    (setf (ball-y-vel ball) (- (ball-y-vel ball))))

  ;; Detect if we collide with player
  (when (player-collision-p ball *player-one*)
    (if (> 0 (ball-x-vel ball))
        (setf (ball-x-vel ball) (- (- (ball-x-vel ball) 0.1)))))

  (when (player-collision-p ball *player-two*)
    (if (< 0 (ball-x-vel ball))
        (setf (ball-x-vel ball) (- (+ (ball-x-vel ball) 0.1)))))

  ;; Detect if we score
  (when (and (> -50 (ball-x-pos ball))
             (> 0 (ball-x-vel ball)))
    (reset-ball ball)
    (incf (player-score *player-two*)))
  (when (and (< 50 (ball-x-pos ball))
             (< 0 (ball-x-vel ball)))
    (reset-ball ball)
    (incf (player-score *player-one*))))

(defun swank-up ()
  (swank::handle-requests swank::*emacs-connection* t))

(defun gl-print (out &key (x 0) (y 0) (r 255) (g 255) (b 255))
  "Fast print method creates an overlay at position (x, y) and prints out in font:font-size."
  (gl:color r g b)
  (%gl:raster-pos-2d x y)
  (cl-glut:bitmap-string glut:+bitmap-helvetica-18+ out))

(defun draw-score (p1 p2)
  (gl-print (format nil "~d" p1)
            :x -25 :y -40)
  (gl-print (format nil "~d" p2)
            :x 25 :y -40))

(defun draw-field ()
  (gl:color 0 1 0)
  (gl:rect -50 -50 50 50)
  (gl:color 0.8 0.8 0.8)
  (gl:rect -26 -26 26 26)
  (gl:color 0.25 1 0.5)
  (gl:rect -25 -25 25 25))

(defun draw-in-play ()
  (gl:color 1 1 1)
  (draw-player *player-one*)
  (draw-player *player-two*)
  (draw-ball *ball*))

(defun draw-victory ()
  (gl-print (format nil *victory*)
            :x -25))

(defmethod render ()
  "Render field in depth order"
  (gl:clear :color-buffer)
  (incf *frames*)
  (gl:with-pushed-matrix
    (draw-field)
    (if (not *game-over*)
        (draw-in-play)
        (draw-victory))
    (draw-score
     (player-score *player-one*)
     (player-score *player-two*))
    (when *debug*
      (gl-print (format nil "FPS: ~$" *fps*)
                :x -25 :y -25
                :g 0 :b 0))))

(defmethod set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

;; TODO: Fix this to randomize a starting velocity for ball
(defun init ()
  (setf *fps*    0)
  (setf *frames* 0)
  (setf *time*   0)
  (setf (player-y-pos *player-one*) 0)
  (setf (player-y-pos *player-two*) 0)
  (setf (player-score *player-one*) 0)
  (setf (player-score *player-two*) 0)
  (setf (ball-x-pos *ball*)         0.0)
  (setf (ball-y-pos *ball*)         0.0)
  (setf (ball-x-vel *ball*)         0.5)
  (setf (ball-y-vel *ball*)         0.5))

(defun update-time ()
  (when (< 1 (- (glfw:get-time) *time*))
    (setf *fps* (/ *frames* (- (glfw:get-time) *time*)))
    (setf *frames* 0)
    (setf *time* (glfw:get-time))))

(defun main ()
  "MONSTER MAIN FUNC"
  (init)
  (glfw:with-init-window (:title  "PLONG"
                                  :width  *width*
                                  :height *height*)
    (glut:init)
    (setf %gl:*gl-get-proc-address* #'%glfw:get-proc-address)
    (glfw:set-key-callback          'key-callback)
    (glfw:set-window-size-callback  'window-size-callback)
    (gl:clear-color                 0 0 0 0)
    (set-viewport                   *width* *height*)
    (setf *time* (glfw:get-time))
    (loop until (glfw:window-should-close-p)
       do (update-time)
       do (render)
       do (update-ball *ball*)
       do (update)
       do (glfw:swap-buffers)
       do (glfw:poll-events)
       do (swank-up))))
