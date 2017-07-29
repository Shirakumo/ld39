#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(defclass main (trial:main)
  ()
  (:default-initargs :clear-color (vec 0.25 0.25 0.25 1)))

(defmethod initialize-instance :after ((main main) &key)
  (harmony-simple:start)
  (setf (harmony:min-distance (harmony-simple:segment :sfx)) 32))

(defmethod finalize :after ((main main))
  (harmony-simple:stop))

(define-subject sidescroll-camera* (sidescroll-camera)
  ()
  (:default-initargs :name :camera))

(defmethod setup-perspective :after ((camera sidescroll-camera*) ev)
  (vsetf (location camera)
         (/ (width ev) 2)
         (/ (height ev) 3/2)))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (enter (make-instance 'player) scene)
      (enter (make-instance 'ground :size (vec 10000 512) :location (vec 0 (+ 64 256) 0)) scene)
      (loop repeat 10 for x from 0 by 256
            do (enter (make-instance 'ground :size (vec 64 128) :location (vec x -128 0)) scene))
      (enter (make-instance 'sidescroll-camera* :target (unit :player scene))
             scene)))
  (maybe-reload-scene))

(define-shader-pass light-scatter-pass* (light-scatter-pass)
  ()
  (:inhibit-shaders (light-scatter-pass :fragment-shader)))

(define-class-shader (light-scatter-pass* :fragment-shader)
  '(ld39 #p"light-scatter.frag"))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass))
          (pass2 (make-instance 'black-render-pass))
          (pass3 (make-instance 'light-scatter-pass* :uniforms `(("origin" ,(vec 0.7 1))))))
      (register pass1 pipeline)
      (connect (port pass1 'color) (port pass3 'previous-pass) pipeline)
      (connect (port pass2 'color) (port pass3 'black-render-pass) pipeline)))
  (maybe-reload-scene))

(defun launch ()
  (trial:launch 'main))
