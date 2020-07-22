;;; Copyright © 2020 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see
;;; <http://www.gnu.org/licenses/>.

(defpackage :cl-raytracer
  (:use :cl :trivia))

(in-package :cl-raytracer)

;; I'm not sure of the best way to set this just for this file :x
(eval-when (:compile-toplevel)
  (setf *read-default-float-format* 'double-float))


;;;
;;; Image encoding.
;;;

(defun write-ppm (width height pixels)
  "Encode the WIDTH by HEIGHT image given as PIXELS into the portable pixmap
format (PPM), writing the result to `(current-output-port)'."
  (write-line "P3")
  (format t "~a ~a~%" width height)
  (write-line "255")
  (loop for (r g b) across pixels
        do (format t "~a ~a ~a~%" r g b)))


;;;
;;; Are mathematical objects in the room with us right now?
;;;

(defun square (n) (* n n))

(defun vec3+ (&rest vecs)
  "Return the sum of VECS, as in vector space addition."
  (reduce #'(lambda (a b)
              (let-match (((vector x1 y1 z1) a)
                          ((vector x2 y2 z2) b))
                (vector (+ x1 x2) (+ y1 y2) (+ z1 z2))))
          vecs
          :initial-value #(0.00 0.00 0.00)))

(defun vec3- (&rest vecs)
  "Return the difference of VECS, as in vector space subtraction."
  (if (zerop (length vecs))
      #(0.00 0.00 0.00)
      (reduce #'(lambda (a b)
                  (let-match (((vector x1 y1 z1) a)
                              ((vector x2 y2 z2) b))
                    (vector (- x1 x2) (- y1 y2) (- z1 z2))))
              (cdr vecs)
              :initial-value (car vecs))))

(defun vec3* (c u)
  "Return the vector U scaled by a constant C, as in vector space scalar
  multiplication."
  (let-match (((vector x y z) u))
    (vector (* c x) (* c y) (* c z))))

(defun vec3-dot (u v)
  "Return the dot product of the vectors U and V."
  (let-match (((vector x1 y1 z1) u)
              ((vector x2 y2 z2) v))
    (+ (* x1 x2) (* y1 y2) (* z1 z2))))

(defun vec3-cross (u v)
  "Return the cross product of the vectors U and V."
  (let-match (((vector x1 y1 z1) u)
              ((vector x2 y2 z2) v))
    (vector (- (* y1 z2) (* z1 y2))
            (- (* z1 x2) (* x1 z2))
            (- (* x1 y2) (* y1 x2)))))

(defun vec3-magnitude (u)
  "Return the magnitude of vector U."
  (let-match (((vector x y z) u))
    (sqrt (+ (square x) (square y) (square z)))))

(defun vec3-normalize (u)
  "Return the normal vector parallel to vector U."
  (vec3* (/ 1 (vec3-magnitude u)) u))

(defun vec3-components (u)
  "Return a list (x y z) of the components of vector U."
  (let-match (((vector x y z) u))
    (list x y z)))

(defstruct ray origin direction)

(defmethod point-at ((r ray) time)
  "Return the position of RAY at time T."
  (with-slots (origin direction) r
    (vec3+ origin (vec3* time direction))))

(defun to-radians (d)
  "Convert D, a value in degrees, to radians."
  (* d (/ PI 180)))


;;;
;;; Shapes and generic procedures for working with them.
;;;

(defstruct plane p0 n material)

(defmethod intersect (ray (p plane) t-min t-max)
  (with-slots (p0 (plane-normal n)) p
    (with-slots (origin direction) ray
      (let* ((normal (vec3-normalize plane-normal))
             (denominator (vec3-dot direction normal)))
        (if (not (zerop denominator))
            (let ((time (/ (vec3-dot (vec3- p0 origin) normal)
                           denominator)))
              (if (<= t-min time t-max)
                  time)))))))

(defmethod normal ((shape plane) position)
  (vec3-normalize (plane-n shape)))

(defmethod material ((shape plane))
  (plane-material shape))

(defstruct sphere center radius material)

(defmethod intersect (ray (s sphere) t-min t-max)
  (with-slots (origin direction) ray
    (with-slots (center radius) s
      (let* ((oc (vec3* (- 1) (vec3- center origin)))
             (A  (vec3-dot direction direction))
             (B  (* 2.0 (vec3-dot oc direction)))
             (C  (- (vec3-dot oc oc) (square radius)))

             (discriminant (- (square B) (* 4 A C)))
             (time (if (plusp discriminant)
                       (let ((p (/ (+ (- B) (sqrt discriminant)) (* 2 A)))
                             (m (/ (- (- B) (sqrt discriminant)) (* 2 A))))
                         (if (>= m t-min) m p))
                       (/ (- B) (* 2 A)))))
        (if (and (not (minusp discriminant))
                 (<= t-min time t-max))
            time)))))

(defmethod normal ((shape sphere) position)
  (vec3-normalize (vec3- position (sphere-center shape))))

(defmethod material ((shape sphere))
  (sphere-material shape))


;;;
;;; Lights, and generic procedures for working with them.
;;;

(defstruct light-sample intensity position direction)
(defstruct spot-light from to intensity exponent cutoff-angle)

(defmethod sample-at ((light spot-light) point)
  (with-slots ((position from) (target to) cutoff-angle intensity) light
    (let* ((direction (vec3- position point))
           (pf (vec3-normalize (vec3- point position)))
           (intensity (if (< (vec3-dot pf (vec3-normalize (vec3- target position)))
                             (cos (to-radians cutoff-angle)))
                          (vector 0.00 0.00 0.00)
                          (vec3* (* (/ 1 (square (vec3-magnitude direction)))
                                    (expt (vec3-dot pf (vec3-normalize (vec3- target position)))
                                          (spot-light-exponent light)))
                                 intensity))))
      (make-light-sample :intensity intensity
                         :position position
                         :direction (vec3-normalize direction)))))


;;;
;;; Scene graph.
;;;

(defparameter *image-width*  1920)
(defparameter *image-height* 1080)
(defparameter *image-aspect-ratio* (/ *image-width* *image-height*))

(defparameter *camera-position* (vector 8.00 5.00 9.00))
(defparameter *camera-target*   (vector 0.25 0.00 0.50))
(defparameter *camera-up*       (vector 0.00 1.00 0.00))
(defparameter *camera-fov*      30)

(defparameter *ambient-light* (vector 0.01 0.01 0.01))
(defparameter *lights* (list (make-spot-light :from (vector 10.00 10.00 5.00)
                                              :to   (vector 0.00 0.00 0.00)
                                              :intensity (vector 100.00 96.00 88.00)
                                              :exponent 50
                                              :cutoff-angle 15)))


;;;
;;; Materials.
;;;

(defstruct material ka kd ks kr kt p ior)
(defun diffuse-material (ka kd)      (make-material :ka ka :kd kd))
(defun phong-material   (ka kd ks p) (make-material :ka ka :kd kd :ks ks :p p))

(defun reflect (l n)
  "Compute reflected vector, by mirroring l around n."
  (vec3- (vec3* (* 2.00 (vec3-dot n l)) n) l))

(defun shade-pixel (shape position origin)
  (with-slots (ka kd ks p) (material shape)
    (let* ((normal (normal shape position))
           (Ia (let-match (((vector x1 y1 z1) ka)
                           ((vector x2 y2 z2) *ambient-light*))
                 (vector (* x1 x2) (* y1 y2) (* z1 z2))))
           (Id (apply #'vec3+
                      (mapcar #'(lambda (light)
                                  (let-match* ((sample (sample-at light position))
                                               (direction (light-sample-direction sample))
                                               (intensity (light-sample-intensity sample))
                                               (scalar (max (vec3-dot normal direction) 0))
                                               ((vector x1 y1 z1) kd)
                                               ((vector x2 y2 z2) intensity))
                                    (vector (* x1 x2 scalar) (* y1 y2 scalar) (* z1 z2 scalar))))
                              *lights*)))
           (Is (if ks
                   (apply #'vec3+
                          (mapcar #'(lambda (light)
                                      (let-match* ((sample (sample-at light position))
                                                   (point  (light-sample-position sample))
                                                   (intensity (light-sample-intensity sample))
                                                   (l (vec3-normalize (vec3- point position)))
                                                   (v (vec3-normalize (vec3- origin position)))
                                                   (r (reflect l normal))
                                                   (scalar (expt (max 0 (vec3-dot v r)) p))
                                                   ((vector x1 y1 z1) ks)
                                                   ((vector x2 y2 z2) intensity))
                                        (vector (* x1 x2 scalar) (* y1 y2 scalar) (* z1 z2 scalar))))
                                  *lights*))
                   (vector 0.00 0.00 0.00))))
      (let-match* (((vector x y z) (vec3+ Ia Id Is)))
        (vector (min 1.0 x) (min 1.0 y) (min 1.0 z))))))

(defparameter *shapes* (list (make-sphere :center (vector -0.25 0.00 0.25)
                                          :radius 1.25
                                          :material (phong-material (vector 1.0 0.2 0.2)
                                                                    (vector 1.0 0.2 0.2)
                                                                    (vector 2.0 2.0 2.0)
                                                                    20))
                             (make-plane :p0 (vector 0.00 -1.25 0.00)
                                         :n (vector 0.00 1.00 0.00)
                                         :material (diffuse-material (vector 1.0 1.0 0.2)
                                                                     (vector 1.0 1.0 0.2)))))

(defun coord-to-ray (x y)
  "Return the ray corresponding to the point X, Y on the viewport plane."
  (let* ((dist 1.0)
         (top    (* dist (tan (/ (to-radians *camera-fov*) 2))))
         (right  (* top *image-aspect-ratio*))
         (bottom (- top))
         (left   (- right))
         (W (vec3-normalize (vec3- *camera-position* *camera-target*)))
         (U (vec3-normalize (vec3-cross *camera-up* W)))
         (V (vec3-cross W U))
         (corner (vec3+ *camera-position*
                        (vec3* left     U)
                        (vec3* bottom   V)
                        (vec3* (- dist) W)))
         (across (vec3* (* 2 right) U))
         (up     (vec3* (* 2 top)   V)))
    (make-ray :origin *camera-position*
              :direction (vec3-normalize
                          (vec3+ corner
                                 (vec3* x across)
                                 (vec3* y up)
                                 (vec3* (- 1) *camera-position*))))))

(defun lerp (a b time)
  "Interpolate between A and B with parameter T."
  (+ (* (- 1.0 time) a) (* time b)))

(defun ray-color (r)
  "Return an arbitrary color for R."
  (let* ((y (vec3-y (ray-direction r)))
         (time (* 0.5 (+ y 1.0))))
    (list (round (* 255 (lerp 1.0 0.5 time)))
          (round (* 255 (lerp 1.0 0.7 time)))
          (round (* 255 (lerp 1.0 1.0 time))))))

(defun ray-intersect-scene (ray)
  "Return the nearest shape with which RAY intersects as (shape .
point), if any. Otherwise, return nil."
  (reduce #'(lambda (a b)
              (cond ((not a) b)
                    ((not b) a)
                    (t (let-match (((list _ t1) a)
                                   ((list _ t2) b))
                         (if (> t1 t2) b a)))))
          (mapcar #'(lambda (shape)
                      (let ((intersection (intersect ray shape 0.001 10000)))
                        (if intersection (list shape intersection))))
                  *shapes*)
          :initial-value nil))

(defun main ()
  (let ((image (make-array (* *image-width* *image-height*) :initial-element '(0 0 0))))
    (flet ((coord-to-index (x y)
             (+ x (* y *image-width*)))
           (screen-to-viewport (x y)
             (list (coerce (/ x *image-width*) 'double-float)
                   (coerce (/ (- *image-height* 1 y) *image-height*) 'double-float)))
           (to-color (u)
             (mapcar #'(lambda (n) (round (* 255 n))) (vec3-components u))))
      (dotimes (x *image-width*)
        (dotimes (y *image-height*)
          (setf (elt image (coord-to-index x y))
                (let ((ray (apply #' coord-to-ray (screen-to-viewport x y))))
                  (match (ray-intersect-scene ray)
                    ((list shape time)
                     (let ((position (point-at ray time)))
                       (to-color (shade-pixel shape position (ray-origin ray)))))
                    (_ (ray-color ray))))))))
    (write-ppm *image-width* *image-height* image)))

(main)
