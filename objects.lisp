#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(defmacro define-object (name &key (size 128) (path (format NIL "~(~a~).png" name)) background)
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
                                            (32 '32x)))))))

(define-object barrel-0)
(define-object barrel-1)
(define-object barrel-2)
(define-object barrel-3)
(define-object barrel-4)
(define-object cables-0 :size 512 :background T)
(define-object cables-1 :size 512 :background T)
(define-object cables-2 :size 512 :background T)
(define-object cables-3 :size 512 :background T)
(define-object cables-4 :size 512 :background T)
(define-object cables-5 :size 512 :background T)
(define-object cables-6 :size 512 :background T)
(define-object cables-7 :size 512 :background T)
(define-object cables-8 :size 512 :background T)
(define-object crate-0)
(define-object crate-1)
(define-object pipe-0 :size 256 :background T)
(define-object pipe-1 :size 256 :background T)
(define-object pipe-2 :size 256 :background T)
(define-object pipe-3 :size 256 :background T)
(define-object tire-0)
(define-object tire-1)
(define-object debris-0 :background T)
(define-object debris-1 :background T)
(define-object debris-2 :background T)
(define-object debris-3 :background T)
(define-object debris-4 :background T)
(define-object debris-5 :background T)
(define-object debris-6 :background T)
(define-object debris-7 :background T)
(define-object debris-8 :background T)
(define-object debris-9 :background T)
(define-object corner-l :size 32 :background T)
(define-object corner-r :size 32 :background T)
