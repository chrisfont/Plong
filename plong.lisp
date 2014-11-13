;;;; plong.lisp

(in-package #:plong)

;;; "plong" goes here. Hacks and glory await!

;; NOTE: Uncomment below to optimize during normal run
;;       Testing showed ~2x speed increase in render code...
;;;;(declaim (optimize speed))

(defvar *debug* nil)

(defvar *victory* "")
(defvar *game-over* nil)

(defvar *width* 600)
(defvar *height* 400)

(defvar *court-width*  100)
(defvar *court-height* 100)

(defvar *player-height* 40)
(defvar *player-width*  2)

(defvar *ball-length* 5)
(defvar *ball-width*  2)
(defvar *ball-height* 2)

(defvar *ball-y-vel-inc* 0.1)
(defvar *ball-x-vel-inc* 0.1)

(defvar *ball-vel-default* 0.5)

(defvar *fps*    0)
(defvar *frames* 0)
(defvar *time*   0)

;; TODO: Make these macros?
(defun get-court-left ()
  (- (/ *court-width* 2)))

(defun get-court-right ()
  (/ *court-width* 2))

(defun get-court-bot ()
  (- (/ *court-height* 2)))

(defun get-court-top ()
  (/ *court-height* 2))

(defstruct player
  "Player structure. Provides both <x,y> positions and score."
  (score 0 :type integer)
  (x-pos 0 :type integer)
  (y-pos 0 :type integer))

(defstruct ball
  "Ball structure. Provides both <x,y> positions and <x,y> velocities."
  (x-pos 0.0 :type float)
  (y-pos 0.0 :type float)
  (x-vel 0.0 :type float)
  (y-vel 0.0 :type float))

(defvar *max-score* 10)

(defvar *player-one* (make-player
                      :x-pos (+ (get-court-left) (/ *player-width* 2))))

(defvar *player-two* (make-player
                      :x-pos (- (get-court-right) (/ *player-width* 2))))

;; TODO: Add ball trail.
(defvar *ball* (make-ball))
(defvar *balls* nil)

(defun draw-ball (ball &key (r 1) (g 1) (b 1) (op 1))
  "Draw input ball."
  (gl:color r g b op)
  (let ((x-start (- (ball-x-pos ball)
                    (/ *ball-width* 2)))
        (x-end   (+ (ball-x-pos ball)
                    (/ *ball-width* 2)))
        (y-start (- (ball-y-pos ball)
                    (/ *ball-height* 2)))
        (y-end   (+ (ball-y-pos ball)
                    (/ *ball-height* 2))))
    (gl:rect x-start y-start x-end y-end)))

(defun draw-balls (balls)
  (let ((op 1))
    (loop for ball in balls
       do (draw-ball ball :op op)
       do (setf op (- op 0.25)))))

(defun draw-player (player)
  "Draw input layer pad."
  (let ((x-start (- (player-x-pos player)
                    (/ *player-width* 2)))
        (x-end   (+ (player-x-pos player)
                    (/ *player-width* 2)))
        (y-start (- (player-y-pos player)
                    (/ *player-height* 2)))
        (y-end   (+ (player-y-pos player)
                    (/ *player-height* 2))))
    (gl:rect x-start y-start x-end y-end)))

(defvar *player-one-action* nil)
(defvar *player-two-action* nil)

(defun player-up (player)
  "Adjust input player up. Maxes at field boundary"
  (let ((y-top (+ (player-y-pos player) (/ *player-height* 2))))
    (when (> (get-court-top) y-top)
      (setf (player-y-pos player) (+ (player-y-pos player) 2)))))

(defun player-down (player)
  "Adjust input player position down. Maxes at field boundary"
  (let ((y-bot (- (player-y-pos player) (/ *player-height* 2))))
    (when (< (get-court-bot) y-bot)
      (setf (player-y-pos player) (- (player-y-pos player) 2)))))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (if (eq action :press)
    (case key
      (:escape (glfw:set-window-should-close))
      (:up     (setf *player-two-action* #'player-up))
      (:down   (setf *player-two-action* #'player-down))
      (:w      (setf *player-one-action* #'player-up))
      (:s      (setf *player-one-action* #'player-down))
      (:y      (setf *debug* (not *debug*)))
      (:enter  (when *game-over*
                 (setf *game-over* (not *game-over*))
                 (init))))
    (cond
      ((or (eq :up key) (eq :down key))
       (setf *player-two-action* nil))
      ((or (eq :w key) (eq :s key))
       (setf *player-one-action* nil)))))

(glfw:def-window-size-callback window-size-callback (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun reset-ball (ball)
  "Clear ball positions and reset original velocities (adjusted for scoring player)"
  (setf (ball-x-pos ball) 0.0)
  (setf (ball-y-pos ball) 0.0)
  (setf (ball-x-vel ball)
        (if (< 0 (ball-x-vel ball))
            (- *ball-vel-default*)
            *ball-vel-default*))
  (setf (ball-y-vel ball)
        (if (< 0 (ball-y-vel ball))
            (- *ball-vel-default*)
            *ball-vel-default*)))

(defun update ()
  "Primary update routine."
  (update-players)
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

(defun update-players ()
  "Update both players. Relies on global states :*"
  (when *player-one-action*
    (funcall *player-one-action* *player-one*))
  (when *player-two-action*
    (funcall *player-two-action* *player-two*)))

(defun player-collision-p (ball player)
  "Detect if input ball is colliding with current player pad."
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

(defun ball-vel-update (ball player)
  "Detects which portion of the player pad is being impacted by the ball to give an appropriate"
  (let ((up-top   (+ (player-y-pos player) (/ *player-height* 2)))
        (up-bot   (+ (player-y-pos player) (/ *player-height* 4)))
        (down-bot (- (player-y-pos player) (/ *player-height* 2)))
        (down-top (- (player-y-pos player) (/ *player-height* 4)))
        (y        (ball-y-pos ball))
        (x-inc    (ball-x-vel ball))
        (y-inc    (ball-y-vel ball)))
    (when (and (< up-top y)
               (> up-bot y))
      (setf y-inc (+ y-inc *ball-y-vel-inc*)))
    (when (and (< down-top y)
               (> down-bot y))
      (setf y-inc (- y-inc *ball-y-vel-inc*)))
    (when (and (> up-bot y)
               (< down-top y))
      (setf x-inc
            (if (< 0 x-inc)
                (+ x-inc *ball-x-vel-inc*)
                (- x-inc *ball-x-vel-inc*))))
    (when (and (> 0 x-inc)
               (eq *player-one* player))
      (setf x-inc (- x-inc)))
    (when (and (< 0 x-inc)
               (eq *player-two* player))
      (setf x-inc (- x-inc)))
    (setf (ball-x-vel ball) x-inc)
    (setf (ball-y-vel ball) y-inc)))

(defun update-balls (ball balls)
  (update-ball ball)
  (let ((len (length balls)))
    (if (<= len *ball-length*)
        (push ball balls)
        (let ((new-balls (cons ball (subseq balls 0 (- len 1)))))
          (setf balls new-balls)))))

(defun update-ball (ball)
  ;; Update ball position
  (setf (ball-y-pos ball)
        (+ (ball-y-pos ball) (ball-y-vel ball)))
  (setf (ball-x-pos ball)
        (+ (ball-x-pos ball) (ball-x-vel ball)))

  ;; Detect if we collide with wall
  (when (and (> (get-court-bot) (ball-y-pos ball))
             (> 0 (ball-y-vel ball)))
    (setf (ball-y-vel ball) (- (ball-y-vel ball))))
  (when (and (< (get-court-top) (ball-y-pos ball))
             (< 0 (ball-y-vel ball)))
    (setf (ball-y-vel ball) (- (ball-y-vel ball))))

  ;; Detect if we collide with player
  (when (player-collision-p ball *player-one*)
    (ball-vel-update ball *player-one*))
  (when (player-collision-p ball *player-two*)
    (ball-vel-update ball *player-two*))

  ;; Detect if we score
  (when (and (> (get-court-bot) (ball-x-pos ball))
             (> 0 (ball-x-vel ball)))
    (reset-ball ball)
    (incf (player-score *player-two*)))
  (when (and (< (get-court-top) (ball-x-pos ball))
             (< 0 (ball-x-vel ball)))
    (reset-ball ball)
    (incf (player-score *player-one*))))

(defun swank-up ()
  "Allow connected REPL to update."
  (swank::handle-requests swank::*emacs-connection* t))

(defun gl-print (out &key (x 0) (y 0) (r 255) (g 255) (b 255))
  "Fast print method creates an overlay at position (x, y) and prints out in font:font-size."
  (gl:color r g b)
  (%gl:raster-pos-2d x y)
  (cl-glut:bitmap-string glut:+bitmap-helvetica-18+ out))

;; TODO: Pull score positions into the player structure?
(defun draw-score (p1 p2)
  "Draw the player's scores."
  (gl-print (format nil "~d" p1)
            :x (/ (get-court-left) 2)
            :y (+ (/ (get-court-bot) 2) (/ (get-court-bot) 4)))
  (gl-print (format nil "~d" p2)
            :x (/ (get-court-right) 2)
            :y (+ (/ (get-court-bot) 2) (/ (get-court-bot) 4))))

(defun draw-field ()
  "Draw our standard field."
  (gl:color 0 1 0)
  (gl:rect (get-court-left) (get-court-bot) (get-court-right) (get-court-top))
  (gl:color 0.8 0.8 0.8)
  (gl:rect (- (/ (get-court-left)  2) 1)
           (- (/ (get-court-bot)   2) 1)
           (+ (/ (get-court-right) 2) 1)
           (+ (/ (get-court-top)   2) 1))
  (gl:color 0.25 1 0.5)
  (gl:rect (/ (get-court-left)  2)
           (/ (get-court-bot)   2)
           (/ (get-court-right) 2)
           (/ (get-court-top)   2)))

(defun draw-in-play ()
  (gl:color 1 1 1)
  (draw-player *player-one*)
  (draw-player *player-two*)
  (draw-balls *balls*))

(defun draw-victory ()
  (gl-print (format nil *victory*)
            :x (/ (get-court-left) 2)))

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
                :x (/ (get-court-left) 2) :y (/ (get-court-bot) 2)
                :g 0 :b 0))))

(defmethod set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho (get-court-left) (get-court-right)
            (get-court-bot)  (get-court-top) -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

;; TODO: Fix this to randomize a starting velocity for ball
(defvar *do-time* t)

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
  (setf (ball-x-vel *ball*)         *ball-vel-default*)
  (setf (ball-y-vel *ball*)         *ball-vel-default*))

(defun update-time ()
  (when (< 1 (- (glfw:get-time) *time*))
    (setf *fps* (/ *frames* (- (glfw:get-time) *time*)))
    (setf *frames* 0)
    (setf *time* (glfw:get-time))))

(defun execute-with-time (func &rest args)
  "Wrapper function to execute given function in a timed environment."
  (when *do-time*
    (format t "Executing with time: ~A~%" func)
    (if args
        (time (funcall func args))
        (time (funcall func))))
  (if args
      (funcall func args)
      (funcall func)))

(defun main ()
  "MONSTER MAIN FUNC"
  (execute-with-time #'init)
  (glfw:with-init-window (:title  "PLONG"
                                  :width  *width*
                                  :height *height*)
    ;; OpenGL/GLUT Init
    (glut:init)
    (setf %gl:*gl-get-proc-address* #'%glfw:get-proc-address)
    (glfw:set-key-callback          'key-callback)
    (glfw:set-window-size-callback  'window-size-callback)
    (gl:clear-color                 0 0 0 0)
    (set-viewport                   *width* *height*)
    (setf *time* (glfw:get-time))
    ;; Primary Loop with debug code
    (if *do-time*
        (loop until (glfw:window-should-close-p)
           do (execute-with-time #'update-time)
           do (execute-with-time #'render)
           do (execute-with-time #'update-balls *ball* *balls*)
           do (execute-with-time #'update)
           do (execute-with-time #'glfw:swap-buffers)
           do (execute-with-time #'glfw:poll-events)
           do (swank-up)
           do (setf *do-time* nil))
        (loop until (glfw:window-should-close-p)
           do (update-time)
           do (render)
           do (update-balls *ball* *balls*)
           do (update)
           do (glfw:swap-buffers)
           do (glfw:poll-events)
           do (swank-up)))))
