#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(define-subject editor (located-entity)
  ((active :initarg :active :accessor active)
   (mode :initform :place :accessor mode)
   (vel :initform (vec 0 0 0) :accessor vel)
   (selected :initform NIL :accessor selected)
   (placeable :initform NIL :accessor placeable)
   (mouse-pos :initform (vec 0 0 0) :accessor mouse-pos))
  (:default-initargs :name :editor
                     :active NIL))

(define-handler (editor key-press) (ev key)
  (case key
    (:backspace
     (setf (active editor) (not (active editor)))
     (cond ((active editor)
            (setf (target (unit :camera *loop*)) editor)
            (setf (location editor) (vcopy (location (unit :player *loop*))))
            (remove-handler (unit :player *loop*) *loop*))
           (T
            (setf (target (unit :camera *loop*)) (unit :player *loop*))
            (add-handler (unit :player *loop*) *loop*))))
    (T
     (when (active editor)
       (case key
         (:w (setf (vy (vel editor)) (- 10.0)))
         (:a (setf (vx (vel editor)) (- 10.0)))
         (:s (setf (vy (vel editor)) (+ 10.0)))
         (:d (setf (vx (vel editor)) (+ 10.0))))
       (case (mode editor)
         (:edit (when (selected editor)
                  (case key
                    (:delete
                     (leave (selected editor) *loop*)
                     (setf (selected editor) NIL)))))
         (:place (when (selected editor)
                   )))))))

(define-handler (editor key-release) (ev key)
  (when (active editor)
    (let ((x (vx (vel editor)))
          (y (vy (vel editor))))
      (case key
        (:w (setf (vy (vel editor)) (max y 0)))
        (:a (setf (vx (vel editor)) (max x 0)))
        (:s (setf (vy (vel editor)) (min y 0)))
        (:d (setf (vx (vel editor)) (min x 0)))))))

(define-handler (editor tick) (ev)
  (nv+ (location editor) (vel editor)))

(define-handler (editor mouse-move) (ev pos)
  (setf (mouse-pos editor) (screen->vec pos (width *context*) (height *context*)))
  (when (and (active editor) (placeable editor))
    (setf (location (placeable editor)) (mouse-pos editor))))

(define-handler (editor mouse-press) (ev button)
  (when (active editor)
    (case button
      (:right
       (case (mode editor)
         (:place
          (setf (mode editor) :edit))
         (:edit
          (setf (mode editor) :place
                (selected editor) NIL)))
       (v:info :editor "Mode: ~a" (mode editor)))
      (:left
       (case (mode editor)
         (:place
          (let ((object (placeable editor)))
            (when object
              (let ((pos (location object)))
                (v:log :warn :editor "Add item ~a to ~a,~a" (name object) (vx pos) (vy pos))))))
         (:edit
          (let ((pos (mouse-pos editor)))
            (v:log :warn :editor "Select item at ~a,~a" (vx pos) (vy pos)))))))))

(defmethod paint ((editor editor) target)
  (when (and (active editor) (eql :place (mode editor)) (placeable editor))
    (paint (placeable editor))))
