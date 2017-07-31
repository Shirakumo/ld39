#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(defclass scene* (scene)
  ((draw-order :initform (make-array 0 :adjustable T :fill-pointer T) :accessor draw-order)))

(defmethod enter :after ((unit layered-unit) (scene scene*))
  (loop for pos from 0 below (length (draw-order scene))
        until (< (layer unit) (layer (aref (draw-order scene) pos)))
        finally (array-utils:vector-push-extend-position unit (draw-order scene) pos)))

(defmethod leave :after ((unit layered-unit) (scene scene*))
  (let ((pos (position unit (draw-order scene))))
    (when pos
      (array-utils:vector-pop-position (draw-order scene) pos))))

(defmethod paint ((scene scene) target)
  (for:for ((unit across (draw-order scene)))
    (paint unit target))
  (paint (unit :controller scene) target))

(defclass main (trial:main)
  ((scene :initform (make-instance 'scene*) :accessor scene)
   (map-files :initarg :map-files :initform () :accessor map-files)
   (map-file :initarg :map-file :initform NIL :accessor map-file))
  (:default-initargs
   :clear-color (vec 0.1 0.1 0.1 0)
   :map-files '("startmap" "map0" "map1" "map2" "map3" "map4" "map5" "endmap")
   :map-file "startmap"))

(define-subject sidescroll-camera* (sidescroll-camera)
  ((view-scale :initform 1.0 :accessor view-scale))
  (:default-initargs :name :camera))

(defmethod setup-perspective :after ((camera sidescroll-camera*) ev)
  (setf (view-scale camera) (/ (width ev) 3000))
  (vsetf (location camera)
         (/ (width ev) (view-scale camera) 2)
         (/ (height ev) (view-scale camera) 3/2)))

(defmethod (setf zoom) :after (zoom (camera sidescroll-camera*))
  (vsetf (location camera)
         (/ (width *context*) (* zoom (view-scale camera)) 2)
         (/ (height *context*) (* zoom (view-scale camera)) 3/2)))

(defmethod project-view ((camera sidescroll-camera*) ev)
  (let ((z (* (view-scale camera) (zoom camera))))
    (reset-matrix *view-matrix*)
    (scale-by z z z *view-matrix*)
    (translate (v- (location camera) (location (target camera))) *view-matrix*)))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (cond ((map-file main)
             (load-map (map-file main)))
            (T
             (loop repeat 10 for x from 0 by 512
                   do (enter (make-instance (alexandria:random-elt '(pipe-0 pipe-1 pipe-2 pipe-3)) :location (vec x -128 0)) scene))
             (enter (make-instance 'ground :size (vec 10000 512) :location (vec 0 (+ 64 256) 0)) scene)
             (loop repeat 10 for x from 0 by 256
                   do (enter (make-instance 'ground :size (vec 64 128) :location (vec x -128 0)) scene))
             (enter (make-instance 'player) scene)
             (enter (make-instance 'editor) scene)
             (enter (make-instance 'sidescroll-camera* :target (unit :player scene))
                    scene)
             (enter (make-instance 'light-timer) scene)
             (enter (make-instance 'light-switch :location (vec -500 -300 0)) scene)))
      (issue scene 'level-begin)))
  (maybe-reload-scene))

(define-shader-pass black-render-pass* (black-render-pass)
  ())

(defmethod paint :around ((entity background-entity) (pass black-render-pass*)))

(define-shader-pass light-scatter-pass* (light-scatter-pass)
  ()
  (:inhibit-shaders (light-scatter-pass :fragment-shader)))

(define-class-shader (light-scatter-pass* :fragment-shader)
  '(ld39 #p"light-scatter.frag"))

(define-handler (light-scatter-pass* tick) (ev)
  (let* ((light (unit :light-timer (scene (window :main))))
         (x (if light (float (/ (duration light) (max-duration light))) 1.0))
         (intensity (ease x 'cubic-out)))
    (setf (uniforms light-scatter-pass*) `(("light" ,intensity)))))

(define-shader-pass fader (simple-post-effect-pass)
  ((fade :initarg :fade :initform 0.0 :accessor fade)
   (action :initform :level-begin :accessor action)))

(define-class-shader (fader :fragment-shader)
  "uniform float opacity = 1.0;
uniform sampler2D previous_pass;

in vec2 tex_coord;
out vec4 color;

void main(){
  color = texture(previous_pass, tex_coord);
  color.rgb = mix(vec3(0.1, 0.1, 0.1), color.rgb, opacity);
  color.a = 1;
}")

(define-handler (fader tick) (ev)
  (case (action fader)
    (:level-begin
     (if (< (fade fader) 1.0)
         (incf (fade fader) 0.005)))
    (:level-complete
     (if (< 0.0 (fade fader))
         (decf (fade fader) 0.005)
         (maybe-reload-scene)))
    (:game-over
     (let ((scene (scene (window :main))))
       (enter (load (make-instance 'game-over-screen)) scene)
       (leave (unit :light-timer scene) scene))
     (setf (action fader) NIL)))
  (setf (uniforms fader) `(("opacity" ,(fade fader)))))

(define-handler (fader level-begin) (ev)
  (setf (fade fader) 0.0)
  (setf (action fader) :level-begin))

(define-handler (fader level-complete) (ev)
  (let ((main (window :main)))
    (setf (map-file main) (elt (map-files main)
                               (mod (1+ (or (position (map-file main) (map-files main) :test #'equal) -1))
                                    (length (map-files main))))))
  (setf (action fader) :level-complete))

(define-handler (fader game-over) (ev)
  (when (action fader)
    (setf (action fader) :game-over)))

(define-action retry ()
  (key-release (one-of key :r))
  (gamepad-release (one-of button :start)))

(define-handler (fader retry) (ev)
  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass))
          (pass2 (make-instance 'black-render-pass*))
          (pass3 (make-instance 'light-scatter-pass*))
          (pass4 (make-instance 'fader)))
      (register pass1 pipeline)
      (cond ((active (unit :editor (scene main)))
             (setf (fade pass4) 1.0)
             (connect (port pass1 'color) (port pass4 'previous-pass) pipeline))
            (T
             (connect (port pass1 'color) (port pass3 'previous-pass) pipeline)
             (connect (port pass2 'color) (port pass3 'black-render-pass) pipeline)
             (connect (port pass3 'color) (port pass4 'previous-pass) pipeline)))))
  (maybe-reload-scene))

(defun launch (&optional map)
  #+harmony
  (unless (harmony-simple:started-p)
    (harmony-simple:start))
  (if map
      (trial:launch 'main :map-file map)
      (trial:launch 'main :width 1280 :height 720)))

#+harmony
(deploy:define-hook (:quit harmony) ()
  (harmony-simple:stop))
