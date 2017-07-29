#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.ld39)

(define-subject editor (located-entity)
  ((active :initarg :active :accessor active)
   (mode :initform :place :accessor mode)
   (velocity :initform (vec 0 0 0) :accessor velocity)
   (selected :initform NIL :accessor selected)
   (placeable :initform NIL :accessor placeable)
   (mouse-pos :initform (vec 0 0 0) :accessor mouse-pos)
   (camera-target :initform NIL :accessor camera-target))
  (:default-initargs :name :editor
                     :active NIL))

(define-handler (editor key-press) (ev key)
  ;; TODO: clean this up
  (if (eql :backspace key)
      (let* ((camera (unit :camera *loop*))
             (old-target (target camera)))
        (v:log :warn :editor "Bluh bluh ~a" (if (active editor) "true" "false"))
        (setf (active editor) (not (active editor))
              (target camera) (if (and (eql editor old-target)
                                       (not (active editor))
                                       (camera-target editor))
                                  (camera-target editor)
                                  editor)
              (camera-target editor) (if (eql old-target editor)
                                         (camera-target editor)
                                         old-target)))
      (when (active editor)
        (case key
          (:w (setf (vy (velocity editor)) (- 2.0)))
          (:a (setf (vx (velocity editor)) (- 2.0)))
          (:s (setf (vy (velocity editor)) (+ 2.0)))
          (:d (setf (vx (velocity editor)) (+ 2.0))))
        (cond ((and (eql :edit (mode editor)) (selected editor))
               (case key
                 (:delete (leave (selected editor) *loop*))))
              ((and (eql :place (mode editor)) (placeable editor))
               (v:log :warn :editor "Change placeable here?"))))))

(define-handler (editor key-release) (ev key)
  (when (active editor)
    (let ((x (vx (velocity editor)))
          (y (vy (velocity editor))))
      (case key
        (:w (setf (vy (velocity editor)) (max y 0)))
        (:a (setf (vx (velocity editor)) (max x 0)))
        (:s (setf (vy (velocity editor)) (min y 0)))
        (:d (setf (vx (velocity editor)) (min x 0)))))))

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
                (selected editor) NIL))))
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
