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

(defstruct vec3 x y z)

(defun vec3+ (&rest vecs)
  "Return the sum of VECS, as in vector space addition."
  (if (zerop (length vecs))
      (make-vec3 :x 0 :y 0 :z 0)
      (reduce #'(lambda (a b)
              (with-slots ((x1 x) (y1 y) (z1 z)) a
                (with-slots ((x2 x) (y2 y) (z2 z)) b
                  (make-vec3 :x (+ x1 x2)
                             :y (+ y1 y2)
                             :z (+ z1 z2)))))
          (cdr vecs)
          :initial-value (car vecs))))

(defun vec3- (&rest vecs)
  "Return the difference of VECS, as in vector space subtraction."
  (if (zerop (length vecs))
      (make-vec3 :x 0 :y 0 :z 0)
      (reduce #'(lambda (a b)
              (with-slots ((x1 x) (y1 y) (z1 z)) a
                (with-slots ((x2 x) (y2 y) (z2 z)) b
                  (make-vec3 :x (- x1 x2)
                             :y (- y1 y2)
                             :z (- z1 z2)))))
          (cdr vecs)
          :initial-value (car vecs))))

(defun vec3* (c u)
  "Return the vector U scaled by a constant C, as in vector space scalar
  multiplication."
  (with-slots (x y z) u
    (make-vec3 :x (* c x) :y (* c y) :z (* c z))))

(defun vec3-cross (u v)
  "Return the cross product of the vectors U and V."
  (with-slots ((x1 x) (y1 y) (z1 z)) u
    (with-slots ((x2 x) (y2 y) (z2 z)) v
      (make-vec3 :x (- (* y1 z2) (* z1 y2))
                 :y (- (* z1 x2) (* x1 z2))
                 :z (- (* x1 y2) (* y1 x2))))))

(defun vec3-magnitude (u)
  "Return the magnitude of vector U."
  (with-slots (x y z) u
    (sqrt (+ (square x) (square y) (square z)))))

(defun vec3-normalize (u)
  "Return the normal vector parallel to vector U."
  (vec3* (/ 1 (vec3-magnitude u)) u))

(defstruct ray origin direction)

(defun ray-point-at (r)
  "Return the position of RAY at time T."
  (with-slots (origin direction) r
    (vec3+ origin (vec3* t direction))))

(defun to-radians (d)
  "Convert D, a value in degrees, to radians."
  (* d (/ PI 360)))


;;;
;;; Scene graph.
;;;

(defvar image-width  1920)
(defvar image-height 1080)
(defvar image-aspect-ratio (/ image-width image-height))

(defvar camera-position (make-vec3 :x 8.00 :y 5.00 :z 9.00))
(defvar camera-target   (make-vec3 :x 0.25 :y 0.00 :z 0.50))
(defvar camera-up       (make-vec3 :x 0.00 :y 1.00 :z 0.00))
(defvar camera-fov      30)

(defun coord-to-ray (x y)
  "Return the ray corresponding to the point X, Y on the viewport plane."
  (let* ((dist 1.0)
         (top    (* dist (tan (to-radians camera-fov))))
         (right  (* top image-aspect-ratio))
         (bottom (- top))
         (left   (- right))
         (W (vec3-normalize (vec3- camera-position camera-target)))
         (U (vec3-normalize (vec3-cross camera-up W)))
         (V (vec3-cross W U))
         (corner (vec3+ camera-position
                        (vec3* left     U)
                        (vec3* bottom   V)
                        (vec3* (- dist) W)))
         (across (vec3* (* 2 right) U))
         (up     (vec3* (* 2 top)   V)))
    (make-ray :origin camera-position
              :direction (vec3-normalize
                          (vec3+ corner
                                 (vec3* x across)
                                 (vec3* y up)
                                 (vec3* (- 1) camera-position))))))

(defun lerp (a b time)
  "Interpolate between A and B with parameter T."
  (+ (* (- 1.0 time) a) (* time b)))

(defun ray-color (r)
  "Return an arbitrary color for R."
  (let* ((y (slot-value (slot-value r 'direction) 'y))
         (time (* 0.5 (+ y 1.0))))
    (list (round (* 255 (lerp 1.0 0.5 time)))
          (round (* 255 (lerp 1.0 0.7 time)))
          (round (* 255 (lerp 1.0 1.0 time))))))

(let ((image (make-array (* image-width image-height) :initial-element '(0 0 0))))
  (flet ((coord-to-index (x y)
           (+ x (* y image-width))))
    (dotimes (x (1- image-width))
      (dotimes (y image-height)
        (setf (elt image (coord-to-index x y))
              (ray-color (coord-to-ray x y))))))
  (write-ppm image-width image-height image))
