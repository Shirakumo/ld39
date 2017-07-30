#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(defmacro define-object (name &key (layer 0) (size 128) (path (format NIL "~(~a~).png" name)) background)
  `(progn (define-asset (ld39 ,name) texture
              (,(pathname path)))
          
          (define-shader-subject ,name (decoration ,@(when background `(background-entity)))
            ()
            (:default-initargs
             :texture (asset 'ld39 ',name)
             :size (vec ,size ,size)
             :vertex-array (asset 'ld39 ',(ecase size
                                            (512 '512x)
                                            (256 '256x)
                                            (128 '128x)
                                            (64 '64x)
                                            (32 '32x)))
             :layer ,layer))))

(define-object barrel-0 :layer 3)
(define-object barrel-1 :layer 7)
(define-object barrel-2 :layer 3)
(define-object barrel-3 :layer 7)
(define-object barrel-4 :layer 3)
(define-object crate-0 :layer 3)
(define-object crate-1 :layer 7)
(define-object tire-0 :layer 3)
(define-object tire-1 :layer 7)
(define-object cables-0 :size 512 :background T :layer 6)
(define-object cables-1 :size 512 :background T :layer 4)
(define-object cables-2 :size 512 :background T :layer 6)
(define-object cables-3 :size 512 :background T :layer 4)
(define-object cables-4 :size 512 :background T :layer 6)
(define-object cables-5 :size 512 :background T :layer 4)
(define-object cables-6 :size 512 :background T :layer 6)
(define-object cables-7 :size 512 :background T :layer 4)
(define-object cables-8 :size 512 :background T :layer 6)
(define-object pipe-0 :size 256 :background T :layer 1)
(define-object pipe-1 :size 256 :background T :layer 1)
(define-object pipe-2 :size 256 :background T :layer 1)
(define-object pipe-3 :size 256 :background T :layer 1)
(define-object pipe-4 :size 256 :background T :layer 1)
(define-object debris-0 :size 256 :background T :layer 6)
(define-object debris-1 :size 256 :background T :layer 6)
(define-object debris-2 :size 256 :background T :layer 6)
(define-object debris-3 :size 256 :background T :layer 6)
(define-object debris-4 :size 256 :background T :layer 6)
(define-object debris-5 :size 256 :background T :layer 6)
(define-object debris-6 :size 256 :background T :layer 6)
(define-object debris-7 :size 256 :background T :layer 6)
(define-object debris-8 :size 256 :background T :layer 6)
(define-object debris-9 :size 256 :background T :layer 6)
(define-object corner-l :size 32 :background T :layer 4)
(define-object corner-r :size 32 :background T :layer 4)
(define-object stripes-l :size 256 :background T :layer 3)
(define-object stripes-r :size 256 :background T :layer 3)
(define-object decal-0 :size 256 :background T :layer 3)
(define-object decal-1 :size 256 :background T :layer 3)
(define-object decal-2 :size 256 :background T :layer 3)
(define-object decal-l :size 256 :background T :layer 3)
(define-object decal-r :size 256 :background T :layer 3)


(define-asset (ld39 game-over-screen) texture
    (#p"gameover.png"))

(define-shader-subject game-over-screen (vertex-subject textured-subject layered-unit unplacable)
  ()
  (:default-initargs :texture (asset 'ld39 'game-over-screen)
                     :vertex-array (asset 'ld39 '512x)
                     :layer 20))

(defmethod paint ((screen game-over-screen) target)
  (with-pushed-matrix ((view-matrix :identity)
                       (projection-matrix :identity)
                       (model-matrix :identity))
    (let ((w (width *context*)) (h (height *context*)))
      (orthographic-projection 0 w h 0 -100 100)
      (translate-by (/ w 2) (/ h 2) 100)
      (call-next-method))))

(define-asset (ld39 puddle) texture
    (#p"puddle.png"))

(define-asset (ld39 puddle-mesh) mesh
    ((make-rectangle 512 32)))

(define-shader-subject puddle (decoration)
  ()
  (:default-initargs :texture (asset 'ld39 'puddle)
                     :size (vec 512 32)
                     :vertex-array (asset 'ld39 'puddle-mesh)
                     :layer 6))

(define-asset (ld39 splash) texture
    (#p"splash.png"))

(define-shader-subject splash (decoration background-entity axis-rotated-entity unplacable)
  ((clock :initform 0 :accessor clock))
  (:default-initargs
   :texture (asset 'ld39 'splash)
   :size (vec 64 64)
   :vertex-array (asset 'ld39 '64x)
   :layer 6
   :axis +vy+))

(define-handler (splash tick) (ev dt)
  (incf (clock splash) dt)
  (when (< 0.3 (clock splash))
    (leave splash *loop*)))

(defmethod paint :before ((splash splash) (target shader-pass))
  (let ((program (shader-program-for-pass target splash)))
    (setf (uniform program "transparency") (coerce (/ (clock splash) 0.3s0) 'single-float))))

(define-class-shader (splash :fragment-shader)
  "uniform float transparency = 0.0;
out vec4 color;

void main(){
   color.a *= 1.0-transparency;
}")
