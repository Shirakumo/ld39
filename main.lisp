#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(defclass main (trial:main)
  ()
  (:default-initargs :clear-color (vec 0.1 0.1 0.1 1)))

(defmethod initialize-instance :after ((main main) &key)
  (harmony-simple:start)
  (setf (harmony:min-distance (harmony-simple:segment :sfx)) 32))

(defmethod finalize :after ((main main))
  (harmony-simple:stop))

(define-subject sidescroll-camera* (sidescroll-camera)
  ()
  (:default-initargs :name :camera))

(defmethod setup-perspective :after ((camera sidescroll-camera*) ev)
  (setf (zoom camera) (/ (width ev) 2048))
  (vsetf (location camera)
         (/ (width ev) (zoom camera) 2)
         (/ (height ev) (zoom camera) 3/2)))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (loop repeat 10 for x from 0 by 512
            do (enter (make-instance (alexandria:random-elt '(pipe-0 pipe-1 pipe-2 pipe-3)) :location (vec x -128 0)) scene))
      (enter (make-instance 'ground :size (vec 10000 512) :location (vec 0 (+ 64 256) 0)) scene)
      (loop repeat 10 for x from 0 by 256
            do (enter (make-instance 'ground :size (vec 64 128) :location (vec x -128 0)) scene))
      (enter (make-instance 'player) scene)
      (enter (make-instance 'editor) scene)
      (enter (make-instance 'sidescroll-camera* :target (unit :player scene))
             scene)))
  (maybe-reload-scene))

(define-shader-pass black-render-pass* (black-render-pass)
  ())

(defmethod paint :around ((entity background-entity) (pass black-render-pass*)))

(define-shader-pass light-scatter-pass* (light-scatter-pass)
  ()
  (:inhibit-shaders (light-scatter-pass :fragment-shader)))

(define-class-shader (light-scatter-pass* :fragment-shader)
  '(ld39 #p"light-scatter.frag"))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass))
          (pass2 (make-instance 'black-render-pass*))
          (pass3 (make-instance 'light-scatter-pass*)))
      (register pass1 pipeline)
      (connect (port pass1 'color) (port pass3 'previous-pass) pipeline)
      (connect (port pass2 'color) (port pass3 'black-render-pass) pipeline)))
  (maybe-reload-scene))

(defun launch ()
  (trial:launch 'main))
