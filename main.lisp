#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(defclass main (trial:main)
  ())

(defmethod initialize-instance :after ((main main) &key)
  (harmony-simple:start)
  (setf (harmony:min-distance (harmony-simple:segment :sfx)) 32))

(defmethod finalize :after ((main main))
  (harmony-simple:stop))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (enter (make-instance 'editor) scene)
      (enter (make-instance 'sidescroll-camera :name :camera
                                               :target (unit :editor scene))
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
