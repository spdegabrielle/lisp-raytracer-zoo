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

(import (rnrs (6))
        (rnrs lists (6)))

(define (println . items)
  (for-each display items)
  (newline))

(define (sum list) (fold-left + 0 list))
(define (square x) (* x x))

(define (some n) (cons 'some n))
(define (none)   'none)

(define (is-some? n) (and (pair? n) (eq? 'some (car n))))
(define (is-none? n) (eq? 'none n))

(define (unwrap n)
  (if (is-some? n)
      (cdr n)
      (error 'unwrap "Tried to unwrap `none'.")))

(define (map-option proc n)
  (if (is-some? n)
      (some (proc (unwrap n)))
      n))

(define-syntax maybe-bind
  (syntax-rules ()
    ((maybe-bind ((name option) ...)
       body)
     (if (for-all is-some? (list option ...))
         (let ((name (unwrap option))
               ...)
           body)))))


;;;
;;; Image encoding.
;;;

;; Encode the WIDTH by HEIGHT image given as PIXELS into the portable pixmap
;; format (PPM), writing the result to `(current-output-port)'.
(define (write-ppm width height pixels)
  (define (delimit-values values)
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
;;; I really don't want to deal with all of my vectors being lists. Also, I'm
;;; going to guess that -- at least in CHICKEN -- this is going to be faster
;;; than passing around list.
;;;

(define-record-type vec3 (fields x y z))

;; Macro for destructuring a vec3.
(define-syntax vec3-bind
  (syntax-rules ()
    ((vec3-bind ((names vec) ...)
       body)
     (let-values ((names (values (vec3-x vec)
                                 (vec3-y vec)
                                 (vec3-z vec)))
                  ...)
       body))))

;; Return the sum of VECS, as in vector space addition.
(define (vec3+ . vecs)
  (define (add u v)
    (vec3-bind (((x1 y1 z1) u) ((x2 y2 z2) v))
      (make-vec3 (+ x1 x2) (+ y1 y2) (+ z1 z2))))
  (if (zero? (length vecs))
      (make-vec3 0 0 0)
      (fold-left add (car vecs) (cdr vecs))))

;; Return the difference of VECS, as in vector space subtraction.
(define (vec3- . vecs)
  (define (sub u v)
    (vec3-bind (((x1 y1 z1) u) ((x2 y2 z2) v))
      (make-vec3 (- x1 x2) (- y1 y2) (- z1 z2))))
  (if (zero? (length vecs))
      (make-vec3 0 0 0)
      (fold-left sub (car vecs) (cdr vecs))))

;; Return the vector U scaled by a constant C, as in vector space scalar
;; multiplication.
(define (vec3* c u)
  (vec3-bind (((x y z) u))
    (make-vec3 (* c x) (* c y) (* c z))))

;; Return the dot product of the vectors U and V.
(define (vec3-dot u v)
  (+ (* (vec3-x u) (vec3-x v))
     (* (vec3-y u) (vec3-y v))
     (* (vec3-z u) (vec3-z v))))

;; Return the cross product of the vectors U and V.
(define (vec3-cross u v)
  (vec3-bind (((x1 y1 z1) u) ((x2 y2 z2) v))
    (make-vec3 (- (* y1 z2) (* z1 y2))
               (- (* z1 x2) (* x1 z2))
               (- (* x1 y2) (* y1 x2)))))

;; Return a list (x y z) of the components of vector U.
(define (vec3->list u)
  (vec3-bind (((x y z) u))
    (list x y z)))

;; Return the magnitude of vector U.
(define (vec3-magnitude u)
  (vec3-bind (((x y z) u))
    (sqrt (+ (square x) (square y) (square z)))))

;; Return the normal vector parallel to vector U.
(define (vec3-normalize u)
  (vec3* (/ 1 (vec3-magnitude u)) u))



(define-record-type ray (fields origin direction))

;; Return the position of RAY at time T.
(define (ray-point-at ray t)
  (vec3+ (ray-origin ray) (vec3* t (ray-direction ray))))

;; Convert D, a value in degrees, to radians.
(define (degrees->radians d)
  (let ((pi 3.1415926535897932384626433))
    (* d (/ pi 360))))

;; Return the ray corresponding to the point X, Y on the viewport plane.
(define (coordinate->ray x y)
  (let* ((dist 1.0)
         (top    (* dist (tan (degrees->radians camera-fov))))
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

(define-record-type plane (fields p0 n material))

(define (intersect-plane ray shape t-min t-max)
  (let* ((normal (vec3-normalize (plane-n shape)))
         (denominator (vec3-dot (ray-direction ray) normal)))
    (if (zero? denominator)
        (none)
        (let ((t (/ (vec3-dot (vec3- (plane-p0 shape) (ray-origin ray))
                              normal)
                    denominator)))
          (if (<= t-min t t-max)
              (some t)
              (none))))))

(define-record-type sphere (fields center radius material))

(define (intersect-sphere ray shape t-min t-max)
  (let* ((oc (vec3- (ray-origin ray) (sphere-center shape)))
         (A  (vec3-dot (ray-direction ray) (ray-direction ray)))
         (B  (* 2.0 (vec3-dot oc (ray-direction ray))))
         (C  (- (vec3-dot oc oc) (square (sphere-radius shape))))

         (discriminant (- (square B) (* 4 A C)))
         (t (if (positive? discriminant)
                (let ((p (/ (+ (- B) (sqrt discriminant)) (* 2 A)))
                      (m (/ (- (- B) (sqrt discriminant)) (* 2 A))))
                  (if (>= m t-min) m p))
                (/ (- B) (* 2 A)))))
    (if (and (not (negative? discriminant))
             (<= t-min t t-max))
        (some t)
        (none))))

;; If RAY intersects SHAPE with T-MIN ≤ t ≤ T-MAX, return (some . t). Otherwise,
;; return 'none.
(define (intersect ray shape t-min t-max)
  (let ((proc (cond ((plane?  shape) intersect-plane)
                    ((sphere? shape) intersect-sphere))))
    (proc ray shape t-min t-max)))

(define (normal-plane shape position)
  (vec3-normalize (plane-n shape)))

(define (normal-sphere shape position)
  (vec3-normalize (vec3- position (sphere-center shape))))

;; Return the normal vector of SHAPE at POSITION.
(define (normal shape position)
  (let ((proc (cond ((plane?  shape) normal-plane)
                    ((sphere? shape) normal-sphere))))
    (proc shape position)))

;; Return the material associated with SHAPE's surface.
(define (get-material shape)
  (let ((proc (cond ((plane?  shape) plane-material)
                    ((sphere? shape) sphere-material))))
    (proc shape)))


;;;
;;; Lights, and generic procedures for working with them.
;;;

(define-record-type light-sample (fields intensity position direction))
(define-record-type spot-light (fields from to intensity exponent cutoff-angle))

(define (spot-light-at light point)
  (let* ((position  (spot-light-from light))
         (target    (spot-light-to   light))
         (direction (vec3- position point))
         (cutoff    (spot-light-cutoff-angle light))
         (pf (vec3-normalize (vec3- point position)))
         (intensity (if (< (vec3-dot pf (vec3-normalize (vec3- target position)))
                           (cos (degrees->radians cutoff)))
                        (make-vec3 0.00 0.00 0.00)
                        (vec3* (* (/ 1 (square (vec3-magnitude direction)))
                                  (expt (vec3-dot pf (vec3-normalize (vec3- target position)))
                                        (spot-light-exponent light)))
                               (spot-light-intensity light)))))
    (make-light-sample intensity position (vec3-normalize direction))))

;; Return the <light-sample> produced by LIGHT at POINT.
(define (light-at light point)
  (let ((proc (cond ((spot-light? light) spot-light-at))))
    (proc light point)))


;;;
;;; Materials.
;;;

(define-record-type material (fields ka kd ks kr kt p ior))
(define (diffuse-material ka kd)      (make-material ka kd '() '() '() '() '()))
(define (phong-material   ka kd ks p) (make-material ka kd ks  '() '() p   '()))

;; Compute reflected vector, by mirroring l around n.
(define (reflect l n)
  (vec3- (vec3* (* 2.00 (vec3-dot n l)) n) l))

(define (shade-pixel shape position origin)
  (let* ((ka (material-ka (get-material shape)))
         (kd (material-kd (get-material shape)))
         (ks (material-ks (get-material shape)))
         (p  (material-p  (get-material shape)))
         (normal (normal shape position))
         (Ia (vec3-bind (((x1 y1 z1) ka)
                         ((x2 y2 z2) ambient-light))
               (make-vec3 (* x1 x2) (* y1 y2) (* z1 z2))))
         (Id (apply vec3+
                    (map (lambda (light)
                           (let* ((sample (light-at light position))
                                  (direction (light-sample-direction sample))
                                  (intensity (light-sample-intensity sample))
                                  (scalar (max (vec3-dot normal direction) 0)))
                             (vec3-bind (((x1 y1 z1) kd)
                                         ((x2 y2 z2) intensity))
                                        (make-vec3 (* x1 x2 scalar)
                                                   (* y1 y2 scalar)
                                                   (* z1 z2 scalar)))))
                         lights)))
         (Is (if (not (null? ks))
                 (apply vec3+
                        (map (lambda (light)
                               (let* ((sample (light-at light position))
                                      (point  (light-sample-position sample))
                                      (intensity (light-sample-intensity sample))
                                      (l (vec3-normalize (vec3- point position)))
                                      (v (vec3-normalize (vec3- origin position)))
                                      (r (reflect l normal))
                                      (scalar (expt (max 0 (vec3-dot v r)) p)))
                                 (vec3-bind (((x1 y1 z1) ks)
                                             ((x2 y2 z2) intensity))
                                   (make-vec3 (* x1 x2 scalar)
                                              (* y1 y2 scalar)
                                              (* z1 z2 scalar)))))
                             lights))
                 (make-vec3 0.00 0.00 0.00))))
    (vec3-bind (((x y z) (vec3+ Ia Id Is)))
      (make-vec3 (min 1.0 x) (min 1.0 y) (min 1.0 z)))))


;;;
;;; Scene graph.
;;;

;; Arbitrarily-chosen parameters for rendering.

(define image-width  1920)
(define image-height 1080)
(define image-aspect-ratio (/ image-width image-height))

(define camera-position (make-vec3  8.00  5.00  9.00))
(define camera-target   (make-vec3  0.25  0.00  0.50))
(define camera-up       (make-vec3  0.00  1.00  0.00))
(define camera-fov      30)

;; Color of light in the scene that does not originate from a light source.
(define ambient-light (make-vec3 0.01 0.01 0.01))
(define lights (list (make-spot-light (make-vec3  10.00  10.00   5.00)
                                      (make-vec3   0.00   0.00   0.00)
                                      (make-vec3 100.00  96.00  88.00)
                                      50
                                      15)))
(define shapes (list (make-sphere (make-vec3 -0.25 0.00 0.25)
                                  1.25
                                  (phong-material (make-vec3 1.0 0.2 0.2)
                                                  (make-vec3 1.0 0.2 0.2)
                                                  (make-vec3 2.0 2.0 2.0)
                                                  20))
                     (make-plane (make-vec3  0.00 -1.25  0.00)
                                 (make-vec3  0.00  1.00  0.00)
                                 (diffuse-material (make-vec3 1.0 1.0 0.2)
                                                   (make-vec3 1.0 1.0 0.2)))))

;; Return the nearest shape with which RAY intersects as (some . (shape .
;; point)), if any. Otherwise, return 'none.
(define (ray-intersect-scene ray)
  (map-option
   (lambda (pair)
     (list (car pair)
           (ray-point-at ray (cadr pair))))
   (fold-left (lambda (a b)
                (cond ((is-none? a) b)
                      ((is-none? b) a)
                      (else
                       (maybe-bind ((t1 a) (t2 b))
                                   (if (> (cadr t1) (cadr t2)) b a)))))
              (none)
              (map (lambda (shape)
                     (let ((intersection (intersect ray shape 0.001 10000)))
                       (map-option (lambda (t)
                                     (list shape t))
                                   intersection)))
                   shapes))))

(let ((image (make-vector (* image-width image-height) '(0 0 0))))
  (define (coordinate->index x y)
    (+ x (* y image-width)))
  (define (screen->viewport x y)
    (values (/ x image-width)
            (/ (- image-height 1 y)
               image-height)))
  (define (vec3->color u)
    (map (lambda (n) (exact (round (* 255 n)))) (vec3->list u)))
  (let loop1 ((x 0))
    (let loop2 ((y 0))
      (unless (>= y image-height)
        (vector-set! image (coordinate->index x y)
                     (let-values (((x y) (screen->viewport x y)))
                       (let* ((ray (coordinate->ray x y))
                              (intersection (ray-intersect-scene ray)))
                         (if (is-some? intersection)
                             (let-values (((shape position) (apply values (unwrap intersection))))
                               (vec3->color (shade-pixel shape position (ray-origin ray))))
                             (list 0 0 0)))))
        (loop2 (+ y 1))))
    (unless (>= x (- image-width 1))
      (loop1 (+ x 1))))
  (write-ppm image-width image-height image))
