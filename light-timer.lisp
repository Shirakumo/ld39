#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Joram Schrijver <i@joram.io>
|#

(in-package #:org.shirakumo.fraf.ld39)

(define-subject light-timer ()
  ((max-duration :initarg :max-duration :accessor max-duration)
   (duration :initarg :duration :accessor duration))
  (:default-initargs
   :name :light-timer
   :max-duration (* 30 60)))

(defmethod initialize-instance :after ((light-timer light-timer) &key max-duration duration)
  (unless duration (setf (duration light-timer) max-duration)))

(define-shader-subject light-switch (base-entity)
  ((switchedp :initform nil :accessor switchedp))
  (:default-initargs
   :texture (asset 'ld39 'tire-1)
   :size (vec 128 128)
   :vertex-array (asset 'ld39 '128x)))

(define-handler (light-timer tick) (ev)
  (setf (duration light-timer) (max 0 (1- (duration light-timer)))))
