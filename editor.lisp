#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(define-action delete-object ()
  (key-press (one-of key :delete :backspace)))

(define-subject editor (located-entity)
  ((active :initarg :active :accessor active)
   (mode :initform :place :accessor mode)
   (vel :initform (vec 0 0 0) :accessor vel)
   (selected :initform NIL :accessor selected)
   (to-place :initform (find-class 'ground) :accessor to-place)
   (start-pos :initform (vec 0 0 0) :accessor start-pos)
   (mouse-pos :initform (vec 0 0 0) :accessor mouse-pos))
  (:default-initargs :name :editor
                     :active NIL))

(define-handler (editor delete-object) (ev)
  (when (selected editor)
    (leave (selected editor) *loop*)
    (setf (selected editor) NIL)))

(define-handler (editor toggle-overlay) (ev)
  (setf (active editor) (not (active editor)))
  (cond ((active editor)
         (setf (target (unit :camera *loop*)) editor)
         (setf (location editor) (vcopy (location (unit :player *loop*))))
         (remove-handler (unit :player *loop*) *loop*)
         (v:info :editor "Activating editor mode."))
        (T
         (setf (selected editor) NIL)
         (setf (target (unit :camera *loop*)) (unit :player *loop*))
         (add-handler (unit :player *loop*) *loop*)
         (v:info :editor "Deactivating editor mode."))))

(define-handler (editor tick) (ev)
  (when (active editor)
    (cond ((retained 'movement :left) (setf (vx (vel editor)) -10))
          ((retained 'movement :right) (setf (vx (vel editor)) +10))
          (T (setf (vx (vel editor)) 0)))
    (cond ((retained 'movement :up) (setf (vy (vel editor)) -10))
          ((retained 'movement :down) (setf (vy (vel editor)) +10))
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
           (load (offload selected))))))))

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
                    (setf (selected editor) entity)
                    (return))))))))))))

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
         (case (mode editor)
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
  (when (active editor)
    (let* ((classes (find-leaf-classes (find-class 'sized-entity)))
           (pos (or (position (to-place editor) classes) 0)))
      (setf pos (mod (+ pos (if (< 0 delta) 1 -1)) (length classes)))
      (setf (to-place editor) (nth pos classes))
      (v:info :editor "Selecting ~a" (to-place editor))
      (update-to-place editor (mouse-pos editor)))))

(defun find-leaf-classes (base-class)
  (if (c2mop:class-direct-subclasses base-class)
      (loop with classes = ()
            for sub in (c2mop:class-direct-subclasses base-class)
            do (dolist (leaf (find-leaf-classes sub))
                 (pushnew leaf classes))
            finally (return classes))
      (list base-class)))

(defun snap (vec size)
  (v* (vapplyf vec round size size size size) size))
