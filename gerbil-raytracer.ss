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

(import :std/iter
        :std/format
        :std/srfi/1)
(export main)

(def (some n) (cons 'some n))
(def (none)   'none)

(def (is-some? n) (and (pair? n) (eq? 'some (car n))))
(def (is-none? n) (eq? 'none n))

(def (unwrap n)
  (if (is-some? n)
      (cdr n)
      (error 'unwrap "Tried to unwrap `none'.")))

(def (map-option proc n)
  (if (is-some? n)
      (some (proc (unwrap n)))
      n))


;;;
;;; Image encoding.
;;;

;; Encode the WIDTH by HEIGHT image given as PIXELS into the portable pixmap
;; format (PPM), writing the result to `(current-output-port)'.
(def (write-ppm width height pixels)
  (def (delimit-values values)
    (cond ((null? values)
           (newline))
          ((= 1 (length values))
           (display (car values))
           (delimit-values (cdr values)))
          (else
           (display (car values))
           (display " ")
           (delimit-values (cdr values)))))

  ;; Magic
  (delimit-values '("P3"))

  ;; Dimensions
  (delimit-values (list width height))

  ;; Depth
  (delimit-values '("255"))

  ;; Image contents
  (for-each delimit-values (vector->list pixels)))


;;;
;;; Are mathematical objects in the room with us right now?
;;;

(def (square x) (* x x))

(defstruct vec3 (x y z))

;; Return the sum of VECS, as in vector space addition.
(def (vec3+ . vecs)
  (fold (lambda (a b)
          (with* (((vec3 x1 y1 z1) a)
                  ((vec3 x2 y2 z2) b))
            (vec3 (+ x1 x2) (+ y1 y2) (+ z1 z2))))
        (vec3 0.00 0.00 0.00)
        vecs))

;; Return the difference of VECS, as in vector space subtraction.
(def (vec3- . vecs)
  (fold (lambda (a b)
          (with* (((vec3 x1 y1 z1) a)
                  ((vec3 x2 y2 z2) b))
            (vec3 (- x1 x2) (- y1 y2) (- z1 z2))))
        (vec3 0.00 0.00 0.00)
        vecs))

;; Return the vector U scaled by a constant C, as in vector space scalar
;; multiplication.
(def (vec3* c u)
  (with ((vec3 x y z) u)
    (vec3 (* c x) (* c y) (* c z))))

;; Return the dot product of the vectors U and V.
(def (vec3-dot u v)
  (with* (((vec3 x1 y1 z1) u)
          ((vec3 x2 y2 z2) v))
    (+ (* x1 x2) (* y1 y2) (* z1 z2))))

;; Return the cross product of the vectors U and V.
(def (vec3-cross u v)
  (with* (((vec3 x1 y1 z1) u)
          ((vec3 x2 y2 z2) v))
    (vec3 (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2)))))

;; Return the magnitude of vector U.
(def (vec3-magnitude u)
  (with ((vec3 x y z) u)
    (sqrt (+ (square x) (square y) (square z)))))

;; Return the normal vector parallel to vector U.
(def (vec3-normalize u)
  (vec3* (/ 1 (vec3-magnitude u)) u))

;; Return a list (x y z) of the components of vector U.
(def (vec3->list u)
  (with ((vec3 x y z) u)
    (list x y z)))

(defstruct ray (origin direction))

;; Return the position of ray R at time T.
(def (ray-point-at r t)
  (with ((ray origin direction) r)
    (vec3+ origin (vec3* t direction))))

;; Convert D, a value in degrees, to radians.
(def (degrees->radians d)
  (let ((pi 3.1415926535897932384626433))
    (* d (/ pi 180))))

;; Return the ray corresponding to the point X, Y on the viewport plane.
(def (coordinate->ray x y)
  (let* ((dist 1.0)
         (top    (* dist (tan (/ (degrees->radians camera-fov) 2))))
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
    (make-ray camera-position
              (vec3-normalize
               (vec3+ corner
                      (vec3* x across)
                      (vec3* y up)
                      (vec3* (- 1) camera-position))))))


;;;
;;; Shapes and generic procedures for working with them.
;;;

(defstruct plane (p0 n material))

(defmethod {intersect plane}
  (lambda (self r t-min t-max)
    (with* (((plane p0 normal _) self)
            ((ray origin direction) r))
      (let ((denominator (vec3-dot direction normal)))
        (if (zero? denominator)
          (none)
          (let ((t (/ (vec3-dot (vec3- p0 origin) normal)
                      denominator)))
            (if (<= t-min t t-max)
              (some t)
              (none))))))))

(defmethod {normal plane}
  (lambda (self position)
    (vec3-normalize (plane-n self))))

(defstruct sphere (center radius material))

(defmethod {intersect sphere}
  (lambda (self r t-min t-max)
    (with* (((sphere center radius _) self)
            ((ray origin direction) r))
      (let* ((oc (vec3- origin center))
             (A  (vec3-dot direction direction))
             (B  (* 2.0 (vec3-dot oc direction)))
             (C  (- (vec3-dot oc oc) (square radius)))
             (discriminant (- (square B) (* 4 A C)))
             (t (if (positive? discriminant)
                  (let ((p (/ (+ (- B) (sqrt discriminant)) (* 2 A)))
                        (m (/ (- (- B) (sqrt discriminant)) (* 2 A))))
                    (if (>= m t-min) m p))
                  (/ (- B) (* 2 A)))))
        (if (and (not (negative? discriminant))
                 (<= t-min t t-max))
          (some t)
          (none))))))

(defmethod {normal sphere}
  (lambda (self position)
    (vec3-normalize (vec3- position (sphere-center self)))))

;; Return the material associated with SHAPE's surface.
(def (get-material shape)
  (let ((proc (cond ((plane?  shape) plane-material)
                    ((sphere? shape) sphere-material))))
    (proc shape)))


;;;
;;; Scene graph.
;;;

;; Arbitrarily-chosen parameters for rendering.

(def image-width  1920)
(def image-height 1080)
(def image-aspect-ratio (/ image-width image-height))

(def camera-position (make-vec3  8.00  5.00  9.00))
(def camera-target   (make-vec3  0.25  0.00  0.50))
(def camera-up       (make-vec3  0.00  1.00  0.00))
(def camera-fov      30)

(define shapes (list (sphere (vec3 -0.25 0.00 0.25)
                             1.25
                             '()
                             ;; (phong-material (make-vec3 1.0 0.2 0.2)
                             ;;                 (make-vec3 1.0 0.2 0.2)
                             ;;                 (make-vec3 2.0 2.0 2.0)
                             ;;                 20)
                             )
                     (plane (vec3  0.00 -1.25  0.00)
                            (vec3  0.00  1.00  0.00)
                            ;; (diffuse-material (make-vec3 1.0 1.0 0.2)
                            ;;                   (make-vec3 1.0 1.0 0.2))
                            '()
                            )))

;; Interpolate between A and B with parameter T.
(def (lerp a b t) (+ (* (- 1.0 t) a) (* t b)))

;; Return an arbitrary color for R.
(def (ray-color r)
  (with ((vec3 x y z) (vec3-normalize (ray-direction r)))
    (let ((t (* 0.5 (+ y 1.0))))
      (list (inexact->exact (round (* 255 (lerp 1.0 0.5 t))))
            (inexact->exact (round (* 255 (lerp 1.0 0.7 t))))
            (inexact->exact (round (* 255 (lerp 1.0 1.0 t))))))))

;; Return the nearest shape with which RAY intersects as (some . (shape .
;; point)), if any. Otherwise, return 'none.
(def (ray-intersect-scene ray)
  (map-option
   (lambda (pair)
     (list (first pair) (ray-point-at ray (second pair))))
   (fold (lambda (a b)
           (cond ((is-none? a) b)
                 ((is-none? b) a)
                 (else
                  (with* ((['some t1] a)
                          (['some t2] b))
                    (if (> (cadr t1) (cadr t2)) b a)))))
         (none)
         (map (lambda (shape)
                (let ((intersection {intersect shape ray 0.001 10000}))
                  (map-option (lambda (t) [shape t]) intersection)))
              shapes))))

(def (main . args)
  (def image (make-vector (* image-width image-height) '(0 0 0)))
  (def (coordinate->index x y)
    (+ x (* y image-width)))
  (def (screen->viewport x y)
    [(/ x image-width)
     (/ (- image-height 1 y)
        image-height)])
  (def (vec3->color u)
    (map (lambda (n) (inexact->exact (round (* 255 n)))) (vec3->list u)))
  (for (x (iota image-width))
    (for (y (iota image-height))
      (let* ((ray (apply coordinate->ray (screen->viewport x y)))
             (intersection (ray-intersect-scene ray)))
        (vector-set! image (coordinate->index x y)
                     (if (is-some? intersection)
                       (with (['some shape position] intersection)
                         [0 0 0])
                       (ray-color ray))))))
  (write-ppm image-width image-height image))
