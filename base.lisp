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

(defgeneric solidp (entity)
  (:method (entity) nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass sized-entity (located-entity)
    ((size :initarg :size :accessor size))
    (:default-initargs :size (vec 128 128)))

  (defclass solid-entity (sized-entity)
    ())

  (defclass background-entity (entity)
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
   :axis +vz+))

(define-shader-subject decoration (vertex-subject
                                   textured-subject
                                   sized-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'ld39 '128x)))

(define-shader-subject ground (vertex-subject
                               colored-subject
                               solid-entity)
  ()
  (:default-initargs
   :color (vec 0 0 0 1)))

(defmethod initialize-instance :after ((ground ground) &key size)
  (setf (vertex-array ground) (make-rectangle (vx size) (vy size))))

(defmethod load progn ((ground ground))
  (change-class (vertex-array ground) 'vertex-array :load T))

(defmethod offload progn ((ground ground))
  (offload (vertex-array ground)))
