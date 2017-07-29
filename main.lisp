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

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (enter (make-instance 'player) scene)
      (loop for y from 256 to 5000 by 384
            do (loop for x from 0 to 5000 by 384
                     do (enter (make-instance 'crate :location (vec x y 0))
                               scene)))
      (enter (make-instance 'sidescroll-camera :name :camera
                                               :location (vec 200 150 -10)
                                               :target (unit :player scene))
             scene)))
  (maybe-reload-scene))


(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))
  (maybe-reload-scene))

(defun launch ()
  (trial:launch 'main))
