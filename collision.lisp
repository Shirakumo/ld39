(in-package #:org.shirakumo.fraf.ld39)

(defun %sig (x)
  (if (< x 0.0s0) -1.0s0 1.0s0))

(defstruct hit
  (a NIL) (b NIL)
  (time 0.0s0 :type single-float)
  (normal (vec 0 0) :type vec2)
  (vel (vec 0 0) :type vec2)
  (pos (vec 0 0) :type vec2))

(defun project-vector (a b)
  (v/ (v* (v. a b) b) (vlength b)))

(defun test-segment-vs-line (seg-pos seg-vel line-a line-b)
  (declare (type vec2 seg-pos seg-vel line-a line-b))
  (with-float-traps-masked ()
    (let* ((dx (- (vx line-a) (vx seg-pos)))
           (dy (- (vy line-a) (vy seg-pos)))
           (det (* 2 (- (* (vx line-b) (vy seg-vel))
                        (* (vy line-b) (vx seg-vel))))))
      (unless (= 0 det)
        (let ((u (/ (- (* dy (vx line-b)) (* dx (vy line-b))) det))
              (v (/ (- (* dy (vx seg-vel)) (* dx (vy seg-vel))) det)))
          (when (and (<= 0 u 1)
                     (<= 0 v 1))
            (let* ((time u)
                   (vel (v* seg-vel time))
                   (pos (v+ seg-pos vel)))
              (make-hit :time time
                        :vel (project-vector (v- seg-vel vel) (v- line-b pos))
                        :pos pos))))))))

(defun test-segment-vs-aabb (seg-pos seg-vel aabb-pos aabb-size)
  (declare (type vec2 seg-pos seg-vel aabb-pos aabb-size))
  (sb-int:with-float-traps-masked (:overflow :underflow :inexact)
    (let* ((scale (vec (if (= 0 (vx seg-vel)) most-positive-single-float (/ (vx seg-vel)))
                       (if (= 0 (vy seg-vel)) most-positive-single-float (/ (vy seg-vel)))))
           (sign (vapply seg-vel %sig))
           (near (v* (v- (v- aabb-pos (v* sign aabb-size)) seg-pos) scale))
           (far  (v* (v- (v+ aabb-pos (v* sign aabb-size)) seg-pos) scale)))
      (unless (or (< (vy far) (vx near))
                  (< (vx far) (vy near)))
        (let ((t-near (max (vx near) (vy near)))
              (t-far (min (vx far) (vy far))))
          (when (and (< t-near 1)
                     (< 0 t-far))
            (let* ((time (alexandria:clamp t-near 0.0s0 1.0s0))
                   (normal (if (< (vy near) (vx near))
                               (vec (- (vx sign)) 0)
                               (vec 0 (- (vy sign)))))
                   (vel (v* seg-vel time)))
              (unless (= 0 (v. normal seg-vel))
                (make-hit :time time
                          :normal normal
                          :vel (if (< (vy near) (vx near))
                                   (project-vector (v- seg-vel vel) (vec 0 1))
                                   (project-vector (v- seg-vel vel) (vec 1 0)))
                          :pos (v+ seg-pos vel))))))))))

(defun test-point-vs-aabb (a-pos b-pos b-size)
  (and (<= (- (vx b-pos) (vx b-size)) (vx a-pos) (+ (vx b-pos) (vx b-size)))
       (<= (- (vy b-pos) (vy b-size)) (vy a-pos) (+ (vy b-pos) (vy b-size)))))

(defun test-aabb-vs-aabb (a-pos a-size b-pos b-size)
  (let* ((d (v- b-pos a-pos))
         (p (v- (v+ a-size b-size) (vabs d)))
         (s (vapply d %sig)))
    (unless (or (<= (vx p) 0)
                (<= (vy p) 0))
      (cond ((< (vx p) (vy p))
             (make-hit :normal (vec (vx s) 0)
                       :vel (vec (* (vx p) (vx s)) 0)
                       :pos (vec (+ (vx a-pos) (* (vx a-size) (vx s)))
                                 (vy b-pos))))
            (T
             (make-hit :normal (vec 0 (vy s))
                       :vel (vec 0 (* (vy p) (vy s)))
                       :pos (vec (vx b-pos)
                                 (+ (vy a-pos) (* (vy a-size) (vy s))))))))))

(defun test-sweep-aabb-vs-aabb (a-pos a-vel a-size b-pos b-vel b-size)
  (let ((vel (v+ a-vel b-vel)))
    (cond ((v= 0 vel)
           (test-aabb-vs-aabb a-pos a-size b-pos b-size))
          (T
           (test-segment-vs-aabb a-pos vel b-pos (v+ a-size b-size))))))

(defgeneric test-collision (a b))

(defmethod test-collision ((a base-entity) (b base-entity))
  (let ((hit (test-sweep-aabb-vs-aabb
              (vxy (location a))
              (vxy (vel a))
              (v/ (size a) 2)
              (vxy (location b))
              (vxy (vel b))
              (v/ (size b) 2))))
    (when hit
      (setf (hit-a hit) a (hit-b hit) b)
      hit)))

(defmethod test-collision ((a base-entity) (b sized-entity))
  (let ((hit (test-sweep-aabb-vs-aabb
              (vxy (location a))
              (vxy (vel a))
              (v/ (size a) 2)
              (vxy (location b))
              (vec 0 0)
              (v/ (size b) 2))))
    (when hit
      (setf (hit-a hit) a (hit-b hit) b)
      hit)))
