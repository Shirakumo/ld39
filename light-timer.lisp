(in-package #:org.shirakumo.fraf.ld39)

(define-subject light-timer ()
  ((max-duration :initarg :max-duration :accessor max-duration)
   (duration :initarg :duration :accessor duration))
  (:default-initargs
   :name :light-timer
   :max-duration (* 30 60)))

(defmethod initialize-instance :after ((light-timer light-timer) &key max-duration duration)
  (unless duration (setf (duration light-timer) max-duration)))

(define-asset (ld39 battery) texture
    (#p"battery.png"))

(define-shader-subject light-switch (base-entity)
  ()
  (:default-initargs
   :layer 4
   :texture (asset 'ld39 'battery)
   :size (vec 64 64)
   :vertex-array (asset 'ld39 '64x)))

(define-handler (light-timer tick) (ev)
  (setf (duration light-timer) (max 0 (1- (duration light-timer)))))
