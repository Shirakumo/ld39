#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Joram Schrijver <i@joram.io>
|#

(in-package #:org.shirakumo.fraf.ld39)

(define-action movement ())

(define-action jump (movement)
  (key-press (one-of key :space))
  (gamepad-press (one-of button :x)))

(define-action start-left (movement)
  (key-press (one-of key :a :left))
  (gamepad-move (eql axis :left-h) (< pos -0.2 old-pos)))

(define-action start-right (movement)
  (key-press (one-of key :d :right))
  (gamepad-move (eql axis :left-h) (< old-pos 0.2 pos)))

(define-action start-up (movement)
  (key-press (one-of key :w :up))
  (gamepad-move (eql axis :left-v) (< pos -0.2 old-pos)))

(define-action start-down (movement)
  (key-press (one-of key :s :down))
  (gamepad-move (eql axis :left-v) (< old-pos 0.2 pos)))

(define-action end-left (movement)
  (key-release (one-of key :a :left))
  (gamepad-move (eql axis :left-h) (< old-pos -0.2 pos)))

(define-action end-right (movement)
  (key-release (one-of key :d :right))
  (gamepad-move (eql axis :left-h) (< pos 0.2 old-pos)))

(define-action end-up (movement)
  (key-release (one-of key :w :up))
  (gamepad-move (eql axis :left-v) (< old-pos -0.2 pos)))

(define-action end-down (movement)
  (key-release (one-of key :s :down))
  (gamepad-move (eql axis :left-v) (< pos 0.2 old-pos)))

(define-retention movement (ev)
  (typecase ev
    (start-left (setf (retained 'movement :left) T))
    (start-right (setf (retained 'movement :right) T))
    (start-up (setf (retained 'movement :up) T))
    (start-down (setf (retained 'movement :down) T))
    (end-left (setf (retained 'movement :left) NIL))
    (end-right (setf (retained 'movement :right) NIL))
    (end-up (setf (retained 'movement :up) NIL))
    (end-down (setf (retained 'movement :down) NIL))))

(define-asset (ld39 player) texture
    (#p"player.png"))

(define-shader-subject player (animated-sprite-subject base-entity solid-entity)
  ((vacc :initarg :vacc :accessor vacc)
   (vdcc-ground :initarg :vdcc-ground :accessor vdcc-ground)
   (vdcc-air :initarg :vdcc-air :accessor vdcc-air)
   (vlim :initarg :vlim :accessor vlim)
   (jump-count :initform 0 :accessor jump-count)
   (max-jump-count :initarg :max-jump-count :accessor max-jump-count)
   (against-wall :initform nil :accessor against-wall)
   (wall-jumping-p :initform nil :accessor wall-jumping-p)
   (animation-tile :initform 0 :accessor anim-tile)
   (sounds :initarg :sounds :accessor sounds))
  (:default-initargs
   :name :player
   :layer 5
   :texture (asset 'ld39 'player)
   :vertex-array (asset 'ld39 '128x)
   :animations '((0.75 12)
                 (1 1)
                 (1 1))
   :max-jump-count 2
   :vacc (vec 0.2 -15 0)
   :vdcc-ground (vec 0.4 0.25 0)
   :vdcc-air (vec 0.2 0.5 0)
   :vlim (vec 15 30 0)
   :sounds (list :footstep-left (pool-path 'ld39 #P"footstep-left.mp3")
                 :footstep-right (pool-path 'ld39 #P"footstep-right.mp3"))))

(define-handler (player jump) (ev key)
  (cond
    ((and (against-wall player)
          (not (zerop (vy (vel player)))))
     (setf (vy (vel player)) (vy (vacc player))
           (vx (vel player)) (case (against-wall player)
                               (:left  (- (vx (vlim player))))
                               (:right (+ (vx (vlim player)))))
           (angle player) (case (against-wall player)
                            (:left PI)
                            (:right 0))
           (wall-jumping-p player) t
           (against-wall player) nil
           (animation player) 0))
    ((and (< (jump-count player) (max-jump-count player))
          (>= (vy (vel player)) 0.0))
     (setf (vy (vel player)) (vy (vacc player)))
     (incf (jump-count player))))
  (when (and (= 1 (animation player)) (v/= 0 (vel player)))
    (setf (animation player) 0)))

(defgeneric hit (a b hit))

(define-handler (player tick) (ev)
  (let* ((vel (vel player))
         (vacc (vacc player))
         (vdcc (if (zerop (vy vel))
                   (vdcc-ground player)
                   (vdcc-air player))))
    (cond ((and (retained 'movement :left)
                (not (wall-jumping-p player)))
           (setf (angle player) PI
                 (animation player) 0)
           (decf (vx vel) (vx vacc))
           (when (< 0 (vx vel))
             (decf (vx vel) (vx vdcc))))
          ((and (retained 'movement :right)
                (not (wall-jumping-p player)))
           (setf (angle player) 0
                 (animation player) 0)
           (incf (vx vel) (vx vacc))
           (when (< (vx vel) 0)
             (incf (vx vel) (vx vdcc))))
          ((< (abs (vx vel)) (vx vdcc))
           (setf (vx vel) 0))
          ((< 0 (vx vel))
           (decf (vx vel) (vx vdcc)))
          (T
           (incf (vx vel) (vx vdcc))))

    (cond ((and (v= 0 vel) (= 0 (jump-count player)))
           (setf (animation player) 1))
          ((= 0 (vy vel))
           (setf (animation player) 0)))
    (when (= (animation player) 0)
      (setf (second (first (animations player)))
            (if (/= 0 (vx vel))
                (* (if (< 0.1 (abs (vy vel)))
                       (/ (vy (vlim player)) (abs (vy vel)) 2)
                       0.48)
                   (/ (vx (vlim player)) (abs (vx vel))))
                1000.0)))

    #+harmony-is-actually-working-now
    (when (and (= (animation player) 0)
               (= 0 (vy (vel player)))
               (/= (vx (tile player)) (anim-tile player)))
      (cond
        ((= (vx (tile player)) 1)
         (harmony-simple:play (getf (sounds player) :footstep-right) :sfx
                              :type 'harmony-mp3:mp3-buffer-source
                              :loop NIL))
        ((= (vx (tile player)) 7)
         (harmony-simple:play (getf (sounds player) :footstep-left) :sfx
                              :type 'harmony-mp3:mp3-buffer-source
                              :loop NIL)))
      (setf (anim-tile player) (vx (tile player)))))

  (incf (vy (vel player)) (if (against-wall player)
                              (vy (vdcc-ground player))
                              (vy (vdcc-air player))))

  (let ((nearest-hit NIL))
    (loop repeat 2
          do (setf nearest-hit NIL)
             (for:for ((entity over *loop*))
               (when (and (not (eql player entity))
                          (solidp entity))
                 (let ((hit (test-collision player entity)))
                   (when (and hit (or (not nearest-hit)
                                      (< (hit-time hit) (hit-time nearest-hit))))
                     (setf nearest-hit hit)))))
          while nearest-hit
          do (hit (hit-a nearest-hit) (hit-b nearest-hit) nearest-hit)))

  (for:for ((timer = (unit :light-timer *loop*))
            (entity over *loop*))
    (while timer)
    (when (and (typep entity 'light-switch)
               (test-collision player entity))
      (setf (duration timer) (max-duration timer))
      (leave entity *loop*)))

  (nvclamp (v- (vlim player)) (vel player) (vlim player))
  (when (plusp (vy (vel player)))
    (setf (jump-count player) (max 1 (jump-count player)))
    (setf (wall-jumping-p player) nil))
  (nv+ (location player) (vel player)))

(defmethod hit ((player player) (entity sized-entity) hit)
  (cond ((< (vy (hit-normal hit)) 0.0)
         (setf (jump-count player) 0
               (vy (vel player)) 0.0
               (against-wall player) nil))
        ((> (vy (hit-normal hit)) 0.0)
         (setf (vy (vel player)) 0.0)
         (against-wall player) nil)
        ((not (zerop (vx (hit-normal hit))))
         (setf (against-wall player) (if (plusp (vx (hit-normal hit)))
                                         :right
                                         :left)
               (animation player) 2)))
  (vsetf (location player)
         (round (vx (hit-pos hit)))
         (round (vy (hit-pos hit)))
         (vz (location player)))
  (vsetf (vel player)
         (vx (hit-vel hit))
         (vy (hit-vel hit))
         0))

(defmethod hit ((player player) (entity nuclear-goop) hit)
  (maybe-reload-scene))
