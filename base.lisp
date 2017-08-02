#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Joram Schrijver <i@joram.io>
|#

(in-package #:org.shirakumo.fraf.ld39)

(define-pool ld39 :base :ld39)

(define-asset (ld39 32x) mesh
    ((make-rectangle 32 32)))

(define-asset (ld39 64x) mesh
    ((make-rectangle 64 64)))

(define-asset (ld39 128x) mesh
    ((make-rectangle 128 128)))

(define-asset (ld39 256x) mesh
    ((make-rectangle 256 256)))

(define-asset (ld39 512x) mesh
    ((make-rectangle 512 512)))

(define-asset (ld39 1024x) mesh
    ((make-rectangle 1024 1024)))

(defgeneric solidp (entity)
  (:method (entity) nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass sized-entity (located-entity layered-unit)
    ((size :initarg :size :accessor size))
    (:default-initargs :size (vec 128 128)))

  (defclass solid-entity (sized-entity)
    ())

  (defclass background-entity (entity)
    ())

  (defclass unplacable (entity)
    ()))

(defmethod solidp ((entity solid-entity))
  T)

(define-shader-subject base-entity (sized-entity
                                    axis-rotated-entity
                                    sprite-subject)
  ((vel :initarg :velocity :accessor vel))
  (:default-initargs
   :velocity (vec 0 0 0)
   :vertex-array (asset 'ld39 '128x)
   :axis +vy+))

(define-shader-subject decoration (vertex-subject
                                   textured-subject
                                   sized-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'ld39 '128x)))

(define-shader-subject resizable-subject (vertex-subject
                                          sized-entity)
  ())

(defmethod load :around ((subject resizable-subject))
  (setf (vertex-array subject) (make-rectangle (vx (size subject)) (vy (size subject))))
  (change-class (vertex-array subject) 'vertex-array :load T)
  (call-next-method))

(defmethod offload progn ((subject resizable-subject))
  (offload (vertex-array subject))
  (setf (vertex-array subject) NIL))

(define-shader-subject ground (resizable-subject
                               colored-subject
                               solid-entity)
  ()
  (:default-initargs
   :color (vec 0 0 0 1)
   :layer 2))

(define-shader-subject nuclear-goop (resizable-subject
                                     colored-subject)
  ()
  (:default-initargs
   :color (vec 29/255 232/255 31/255 0.8)
   :size (vec 64 64)
   :vertex-array (asset 'ld39 '64x)
   :layer 6))

(defclass camera-target (located-entity)
  ())

(define-shader-subject exit (resizable-subject colored-subject)
  ((camera-target :initform nil :accessor camera-target))
  (:default-initargs
   :color (vec 0 0 1 1)
   :layer 2))

(defclass game-over (event)
  ())

(defclass level-complete (event)
  ())

(defclass level-begin (event)
  ())
