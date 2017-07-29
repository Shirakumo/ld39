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
    (#p"walk.png"))

(define-shader-subject player (animated-sprite-subject base-entity solid-entity)
  ((vacc :initarg :vacc :accessor vacc)
   (vdcc :initarg :vdcc :accessor vdcc)
   (vlim :initarg :vlim :accessor vlim)
   (jump-count :initform 0 :accessor jump-count)
   (max-jump-count :initarg :max-jump-count :accessor max-jump-count))
  (:default-initargs
   :name :player
   :texture (asset 'ld39 'player)
   :vertex-array (asset 'ld39 '128x)
   :animations '((0.75 12))
   :max-jump-count 2
   :vacc (vec 0.2 -15 0)
   :vdcc (vec 0.4 0.5 0)
   :vlim (vec 15 30 0)))

(define-handler (player jump) (ev key)
  (when (and (< (jump-count player) (max-jump-count player))
             (>= (vy (vel player)) 0.0))
    (setf (vy (vel player)) (vy (vacc player)))
    (incf (jump-count player))))

(defgeneric hit (a b hit))

(define-handler (player tick) (ev)
  (let ((vel (vel player)))
    (cond ((retained 'movement :left)
           (setf (angle player) PI)
           (decf (vx vel) (vx (vacc player)))
           (when (< 0 (vx vel))
             (decf (vx vel) (vx (vdcc player)))))
          ((retained 'movement :right)
           (setf (angle player) 0)
           (incf (vx vel) (vx (vacc player)))
           (when (< (vx vel) 0)
             (incf (vx vel) (vx (vdcc player)))))
          ((< (abs (vx vel)) (vx (vdcc player)))
           (setf (vx vel) 0))
          ((< 0 (vx vel))
           (decf (vx vel) (vx (vdcc player))))
          (T
           (incf (vx vel) (vx (vdcc player)))))

    (setf (second (first (animations player)))
          (if (/= 0 (vx vel))
              (* (if (< 0.1 (abs (vy vel)))
                     (/ (vy (vlim player)) (abs (vy vel)) 2)
                     0.48)
                 (/ (vx (vlim player)) (abs (vx vel))))
              1000.0)))
  
  (incf (vy (vel player)) (vy (vdcc player)))
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
  (nvclamp (v- (vlim player)) (vel player) (vlim player))
  (nv+ (location player) (vel player)))

(defmethod hit ((player player) (entity sized-entity) hit)
  (when (< (vy (hit-normal hit)) 0.0)
    (setf (jump-count player) 0
          (vy (vel player)) 0.0))
  (when (> (vy (hit-normal hit)) 0.0)
    (setf (vy (vel player)) 0.0))
  (vsetf (location player)
         (round (vx (hit-pos hit)))
         (round (vy (hit-pos hit)))
         (vz (location player)))
  (vsetf (vel player)
         (vx (hit-vel hit))
         (vy (hit-vel hit))
         0))
