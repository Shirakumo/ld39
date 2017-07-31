#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(define-action delete-object ()
  (key-press (one-of key :delete :backspace)))

(define-action save-map ()
  (key-press (one-of key :f12 :l2)))

(define-action load-map ()
  (key-press (one-of key :f11 :l1)))

(define-action toggle-editor (system-action)
  (key-press (one-of key :section :grave))
  (gamepad-press (one-of button :home)))

(define-subject editor (located-entity)
  ((active :initarg :active :accessor active)
   (layer :initform NIL :accessor layer)
   (mode :initform :place :accessor mode)
   (vel :initform (vec 0 0 0) :accessor vel)
   (selected :initform NIL :accessor selected)
   (to-place :initform (find-class 'ground) :accessor to-place)
   (start-pos :initform (vec 0 0 0) :accessor start-pos)
   (mouse-pos :initform (vec 0 0 0) :accessor mouse-pos)
   (drag-from :initform NIL :accessor drag-from))
  (:default-initargs :name :editor
                     :active NIL))

(define-handler (editor delete-object) (ev)
  (when (selected editor)
    (leave (selected editor) *loop*)
    (setf (selected editor) NIL)))

(define-handler (editor save-map) (ev)
  (let ((map-file (or (map-file (window :main))
                       (format *standard-output* "Save to > ")
                       (read-line *standard-input*))))
    (save-map map-file)
    (v:log :info :editor "Saved map ~a" map-file)))

(define-handler (editor load-map) (ev)
  (format *standard-output* "Load from > ")
  (setf (map-file (window :main)) (pathname (read-line *standard-input*)))
  (maybe-reload-scene))

(define-handler (editor toggle-editor) (ev)
  (setf (active editor) (not (active editor)))
  (setup-pipeline (window :main))
  (cond ((active editor)
         (setf (mode editor) :edit)
         (setf (target (unit :camera *loop*)) editor)
         (setf (location editor) (vcopy (location (unit :player *loop*))))
         (remove-handler (unit :player *loop*) *loop*)
         (setf (clear-color (window :main)) (vec 0.3 0.3 0.3 0))
         (v:info :editor "Activating editor mode."))
        (T
         (when (and (selected editor) (find (mode editor) '(:place :size)))
           (leave (selected editor) *loop*))
         (setf (selected editor) NIL)
         (setf (target (unit :camera *loop*)) (unit :player *loop*))
         (setf (zoom (unit :camera *loop*)) 1.0)
         (add-handler (unit :player *loop*) *loop*)
         (setf (clear-color (window :main)) (vec 0.1 0.1 0.1 0))
         (v:info :editor "Deactivating editor mode."))))

(define-handler (editor key-release) (ev key)
  (when (active editor)
    (case key
      (:0 (setf (layer editor) NIL))
      (:1 (setf (layer editor) 1))
      (:2 (setf (layer editor) 2))
      (:3 (setf (layer editor) 3))
      (:4 (setf (layer editor) 4))
      (:5 (setf (layer editor) 5))
      (:6 (setf (layer editor) 6))
      (:7 (setf (layer editor) 7))
      (:8 (setf (layer editor) 8))
      (:9 (setf (layer editor) 9)))
    (when (find key '(:0 :1 :2 :3 :4 :5 :6 :7 :8 :9))
      (v:info :editor "Layer changed to ~a" (layer editor)))))

(define-handler (editor tick) (ev)
  (when (active editor)
    (let ((speed (/ 20 (zoom (unit :camera *loop*)))))
      (cond ((retained 'movement :left) (setf (vx (vel editor)) (- speed)))
            ((retained 'movement :right) (setf (vx (vel editor)) (+ speed)))
            (T (setf (vx (vel editor)) 0)))
      (cond ((retained 'movement :up) (setf (vy (vel editor)) (- speed)))
            ((retained 'movement :down) (setf (vy (vel editor)) (+ speed)))
            (T (setf (vy (vel editor)) 0))))
    (nv+ (location editor) (vel editor))))

(define-handler (editor mouse-move) (ev pos)
  (when (active editor)
    (let ((pos (snap (screen->vec pos (width *context*) (height *context*))
                     (if (eql :resize (mode editor)) 16 32)))
          (selected (selected editor)))
      (setf (mouse-pos editor) pos)
      (when selected
        (case (mode editor)
          ((:drag :place)
           (setf (location selected) (transform-pos selected pos)))
          (:resize
           (setf (size selected) (vmax 32 (nvabs (vxy (v* (v- pos (location selected)) 2)))))
           (load (offload selected)))
          (:size
           (setf (location selected) (v/ (v+ pos (start-pos editor)) 2))
           (setf (size selected) (vmax 32 (nvabs (vxy (v- pos (start-pos editor))))))
           (load (offload selected)))
          (T (when (and (drag-from editor) (< 32 (vlength (v- pos (drag-from editor)))))
               (setf (mode editor) :drag))))))))

(defun transform-pos (selected pos)
  (let ((vec (nv/ (vxy_ (size selected)) 2)))
    (incf (vx vec) (vx pos))
    (setf (vy vec) (- (vy pos) (vy vec)))
    vec))

(define-handler (editor mouse-press) (ev button)
  (when (active editor)
    (let ((pos (mouse-pos editor)))
      (case button
        (:right
         (case (mode editor)
           (:place
            (leave (selected editor) *loop*)
            (setf (mode editor) :edit))
           (:edit
            (setf (selected editor) NIL)
            (setf (mode editor) :place)
            (update-to-place editor pos)))
         (v:info :editor "Changed mode to ~a" (mode editor)))
        (:left
         (case (mode editor)
           (:place
            (setf (start-pos editor) pos))
           (:edit
            (cond ((and (or (retained 'key :left-shift) (retained 'key :right-shift)
                            (retained 'key :shift-l) (retained 'key :shift-r))
                        (typep (selected editor) 'resizable-subject))
                   (setf (mode editor) :resize))
                  (T
                   (for:for ((entity over *loop*))
                     (when (and (typep entity 'sized-entity)
                                (not (eql entity (selected editor))))
                       (let ((s (nv/ (vxy_ (size entity)) 2)))
                         (when (v<= (v- (location entity) s)
                                    pos
                                    (v+ (location entity) s))
                           (v:log :info :editor "Selected ~a" entity)
                           (setf (selected editor) entity
                                 (drag-from editor) pos)
                           (return)))))))
            (when (selected editor)
              (setf (drag-from editor) pos)))))))))

(defun update-to-place (editor pos)
  (with-accessors ((selected selected)) editor
    (when selected
      (leave selected *loop*))
    (setf selected (load (if (layer editor)
                             (make-instance (to-place editor) :layer (layer editor))
                             (make-instance (to-place editor)))))
    (setf (location selected) (transform-pos selected pos))
    (enter selected *loop*)))

(define-handler (editor mouse-release) (ev button)
  (when (active editor)
    (let ((pos (mouse-pos editor)))
      (case button
        (:left
         (when (drag-from editor)
           (setf (drag-from editor) NIL))
         (case (mode editor)
           (:drag (setf (mode editor) :edit))
           (:place
            (when (selected editor)
              (setf (start-pos editor) pos)
              (cond ((c2mop:subclassp (to-place editor) (find-class 'resizable-subject))
                     (setf (mode editor) :size))
                    (T
                     (setf (selected editor) NIL)))
              (update-to-place editor pos)))
           (:resize
            (setf (mode editor) :edit))
           (:size
            (setf (selected editor) NIL)
            (setf (mode editor) :place)
            (update-to-place editor pos))))))))

(define-handler (editor mouse-scroll) (ev delta)
  (when (and (active editor))
    (cond ((or (retained 'key :control) (retained 'key :left-control) (retained 'key :right-control)
               (retained 'key :control-l) (retained 'key :control-r))
           (let ((camera (unit :camera *loop*)))
             (setf (zoom camera) (* (zoom camera) (if (< 0 delta) 1.5 (/ 1.5))))))
          ((eql :place (mode editor))
           (let* ((classes (find-leaf-classes (find-class 'sized-entity)))
                  (pos (or (position (to-place editor) classes) 0)))
             (setf pos (mod (+ pos (if (< 0 delta) 1 -1)) (length classes)))
             (setf (to-place editor) (nth pos classes))
             (v:info :editor "Selecting ~a" (to-place editor))
             (update-to-place editor (mouse-pos editor)))))))

(defun find-leaf-classes (base-class)
  (if (c2mop:class-direct-subclasses base-class)
      (loop with classes = ()
            for sub in (c2mop:class-direct-subclasses base-class)
            do (dolist (leaf (find-leaf-classes sub))
                 (unless (or (eql leaf (find-class 'player))
                             (c2mop:subclassp leaf (find-class 'unplacable)))
                   (pushnew leaf classes)))
            finally (return classes))
      (list base-class)))

(defun snap (vec size)
  (v* (vapplyf vec round size size size size) size))

(defun save-map (map &optional (scene *loop*))
  (let ((unit-data (for:for ((unit in (sized-units scene))
                             (data collecting (list (class-name (class-of unit))
                                                    (name unit)
                                                    (vx (location unit))
                                                    (vy (location unit))
                                                    (vz (location unit))
                                                    (vx (size unit))
                                                    (vy (size unit)))))
                     (returning data)))
        (timer-data (for:for ((unit over scene))
                      (when (typep unit 'light-timer)
                        (return (list (class-name (class-of unit))
                                      (name unit)
                                      (max-duration unit)
                                      (duration unit))))))
        (map-path (pool-path 'ld39 (format NIL "~a.lisp" map))))
    (when (or unit-data timer-data)
      (with-open-file (stream map-path :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
        (format stream "~S~%~S~%"
                unit-data
                timer-data)))))

(defun load-map (map &optional (scene *loop*))
  (let (units timer)
    (with-open-file (stream (pool-path 'ld39 (format NIL "~a.lisp" map)))
      (let ((data (make-string (file-length stream)))
            (pos 0))
        (read-sequence data stream)
        (setf (values units pos) (read-from-string data t nil :start pos)
              (values timer pos) (read-from-string data nil nil :start pos))))
    (clear scene)
    (let ((player))
      (for:for (((class name loc-x loc-y loc-z size-x size-y) in units))
        (let ((unit (make-instance class
                                   :name name
                                   :location (vec loc-x loc-y loc-z)
                                   :size (vec size-x size-y))))
          (if (eql name :player)
              (setf player unit)
              (enter unit scene))))
      (when timer
        (destructuring-bind (class name max-duration duration)
            timer
          (enter (make-instance class
                                :name name
                                :max-duration max-duration
                                :duration duration)
                 scene)))
      (enter player scene))
    (enter (make-instance 'editor) scene)
    (enter (make-instance 'sidescroll-camera* :target (unit :player scene))
           scene)
    (load scene)))

(defun sized-units (scene)
  (let ((superclass (find-class 'sized-entity))
        (units))
    (for:for ((unit in (units scene)))
      (when (and (c2mop:subclassp (class-of unit) superclass)
                 (not (c2mop:subclassp (class-of unit) (find-class 'unplacable))))
        (push unit units)))
    units))
