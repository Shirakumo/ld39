#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Joram Schrijver <i@joram.io>
|#

(in-package #:org.shirakumo.fraf.ld39)

(define-pool ld39 :base :ld39)

(define-asset (ld39 128x) mesh
    ((make-rectangle 128 128)))

(define-asset (ld39 32x) mesh
    ((make-rectangle 32 32)))

(defgeneric solidp (entity)
  (:method (entity) nil))

(define-shader-subject base-entity (located-entity
                                    axis-rotated-entity
                                    sprite-subject)
  ((vel :initarg :velocity :accessor vel)
   (solidp :initarg :solidp :accessor solidp))
  (:default-initargs
   :size (vec 128 128)
   :velocity (vec 0 0 0)
   :solidp t
   :vertex-array (asset 'ld39 '128x)
   :axis +vz+))
