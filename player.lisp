#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Joram Schrijver <i@joram.io>
|#

(in-package #:org.shirakumo.fraf.ld39)

(define-asset (ld39 tire) texture
    (#p"tire-1.png"))

(define-asset (ld39 crate) texture
    (#p"crate-0.png"))

(define-shader-subject crate (base-entity solid-entity)
  ()
  (:default-initargs
   :texture (asset 'ld39 'crate)))

(define-action jump ()
  (key-press (one-of key :space)))

(define-shader-subject player (base-entity solid-entity)
  ((move-vel :initform (vec 0 0 0)
             :accessor move-vel)
   (jumpingp :initform nil
             :accessor jumpingp)
   (jump-count :initarg :jump-count
               :initform 0
               :accessor jump-count)
   (max-jump-count :initarg :max-jump-count
                   :initform 2
                   :accessor max-jump-count)
   (jump-speed :initarg :jump-speed
               :initform -5
               :accessor jump-speed))
  (:default-initargs :name :player
                     :texture (asset 'ld39 'tire)
                     :vertex-array (asset 'ld39 '128x)))

(defun jump (player)
  (when (and (< (jump-count player) (max-jump-count player))
             (>= (vy (move-vel player)) 0.0))
    (setf (vy (move-vel player)) (jump-speed player))
    (incf (jump-count player))))

(defun apply-gravity (player)
  (with-accessors ((vy vy)) (move-vel player)
    (setf vy (min (+ vy 0.15) 9.81))))

(define-handler (player key-press) (ev key)
  (case key
    (:a (setf (vx (move-vel player)) (- 3.5)))
    (:d (setf (vx (move-vel player)) (+ 3.5)))
    (:space (setf (jumpingp player) t))))

(define-handler (player key-release) (ev key)
  (case key
    (:a (setf (vx (move-vel player)) (max 0 (vx (move-vel player)))))
    (:d (setf (vx (move-vel player)) (min 0 (vx (move-vel player)))))
    (:space (setf (jumpingp player) nil))))

(defgeneric hit (a b hit))

(define-handler (player tick) (ev)
  (when (jumpingp player) (jump player))
  (apply-gravity player)
  (vsetf (vel player)
         (vx (move-vel player))
         (vy (move-vel player)))
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
  (nv+ (location player) (vel player)))

(defmethod hit ((player player) (entity sized-entity) hit)
  (when (< (vy (hit-normal hit)) 0.0)
    (setf (jump-count player) 0
          (vy (move-vel player)) 0.0
          (vy (vel player)) 0.0))
  (when (> (vy (hit-normal hit)) 0.0)
    (setf (vy (move-vel player)) 0.0))
  (vsetf (location player)
         (vx (hit-pos hit))
         (vy (hit-pos hit))
         (vz (location player)))
  (vsetf (vel player)
         (vx (hit-vel hit))
         (vy (hit-vel hit))
         0))
