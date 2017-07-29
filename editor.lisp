#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(define-action delete-object ()
  (key-press (one-of key :delete :backspace)))

(define-action save-map ()
  (key-press (one-of key :f12)))

(define-action load-map ()
  (key-press (one-of key :f11)))

(define-subject editor (located-entity)
  ((active :initarg :active :accessor active)
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
  (format *standard-output* "Save to > ")
  (save-map (read-line *standard-input*)))

(define-handler (editor load-map) (ev)
  (format *standard-output* "Load from > ")
  (load-map (read-line *standard-input*)))

(define-handler (editor toggle-overlay) (ev)
  (setf (active editor) (not (active editor)))
  (cond ((active editor)
         (setf (mode editor) :edit)
         (setf (target (unit :camera *loop*)) editor)
         (setf (location editor) (vcopy (location (unit :player *loop*))))
         (remove-handler (unit :player *loop*) *loop*)
         (v:info :editor "Activating editor mode."))
        (T
         (when (and (selected editor) (find (mode editor) '(:place :resize)))
           (leave (selected editor) *loop*))
         (setf (selected editor) NIL)
         (setf (target (unit :camera *loop*)) (unit :player *loop*))
         (setf (zoom (unit :camera *loop*)) 1.0)
         (add-handler (unit :player *loop*) *loop*)
         (v:info :editor "Deactivating editor mode."))))

(define-handler (editor tick) (ev)
  (when (active editor)
    (cond ((retained 'movement :left) (setf (vx (vel editor)) -20))
          ((retained 'movement :right) (setf (vx (vel editor)) +20))
          (T (setf (vx (vel editor)) 0)))
    (cond ((retained 'movement :up) (setf (vy (vel editor)) -20))
          ((retained 'movement :down) (setf (vy (vel editor)) +20))
          (T (setf (vy (vel editor)) 0)))
    (nv+ (location editor) (vel editor))))

(define-handler (editor mouse-move) (ev pos)
  (when (active editor)
    (let ((pos (snap (screen->vec pos (width *context*) (height *context*)) 32))
          (selected (selected editor)))
      (setf (mouse-pos editor) pos)
      (when selected
        (case (mode editor)
          (:place
           (setf (location selected) pos))
          (:resize
           (setf (location selected) (v/ (v+ pos (start-pos editor)) 2))
           (setf (size selected) (vmax 32 (nvabs (vxy (v- pos (start-pos editor))))))
           (load (offload selected)))
          (:drag
           (setf (location selected) pos))
          (T (when (and (drag-from editor) (< 16 (vlength (v- pos (drag-from editor)))))
               (setf (mode editor) :drag))))))))

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
                    (return)))))
            (when (selected editor)
              (setf (drag-from editor) pos)))))))))

(defun update-to-place (editor pos)
  (when (selected editor)
    (leave (selected editor) *loop*))
  (setf (selected editor) (load (make-instance (to-place editor) :location pos)))
  (enter (selected editor) *loop*))

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
              (setf (location (selected editor)) pos)
              (setf (start-pos editor) pos)
              (cond ((eql (find-class 'ground) (to-place editor))
                     (setf (mode editor) :resize))
                    (T
                     (setf (selected editor) NIL)))
              (update-to-place editor pos)))
           (:resize
            (setf (selected editor) NIL)
            (setf (mode editor) :place)
            (update-to-place editor pos))))))))

(define-handler (editor mouse-scroll) (ev delta)
  (when (and (active editor))
    (cond ((or (retained 'key :control) (retained 'key :left-control))
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
                 (unless (eql leaf (find-class 'player))
                   (pushnew leaf classes)))
            finally (return classes))
      (list base-class)))

(defun snap (vec size)
  (v* (vapplyf vec round size size size size) size))

(defun save-map (map)
  (let ((data (for:for ((unit in (sized-units *loop*))
                        (data collecting (list (class-name (class-of unit))
                                               (name unit)
                                               (vx (location unit))
                                               (vy (location unit))
                                               (vz (location unit))
                                               (vx (size unit))
                                               (vy (size unit)))))
                (returning data)))
        (map-path (pool-path 'ld39 map)))
    (when data
      (with-open-file (stream map-path :direction :output
                                       :if-exists :overwrite)
        (format stream "~s" data)))))

(defun load-map (map)
  (let ((units (with-open-file (stream (pool-path 'ld39 map))
                 (let ((data (make-string (file-length stream))))
                   (read-sequence data stream)
                   (read-from-string data))))
        (scene *loop*))
    (clear scene)
    (for:for (((class name loc-x loc-y loc-z size-x size-y) in units))
      (enter (make-instance (find-symbol (string class) 'ld39)
                            :name name
                            :location (vec loc-x loc-y loc-z)
                            :size (vec size-x size-y))
             scene))
    (enter (make-instance 'editor) scene)
    (enter (make-instance 'sidescroll-camera* :target (unit :player scene))
           scene)
    (load scene)))

(defun sized-units (scene)
  (let ((superclass (class-name (find-class 'sized-entity)))
        (units))
    (for:for ((unit in (units scene))
              (class = (class-name (class-of unit))))
      (when (subtypep class superclass)
        (push unit units)))
    units))
