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
;;; Procedures that should be standard, but aren't.
;;;

(define (square x) (* x x))

(define (reduce proc list init)
  (define (reduce-iter list result)
    (if (null? list)
        result
        (reduce-iter (cdr list) (proc result (car list)))))
  (reduce-iter list init))


;;;
;;; Image encoding.
;;;

(define (write-ppm width height pixels)
  "Encode the WIDTH by HEIGHT image given as PIXELS into the portable pixmap
format (PPM), writing the result to `(current-output-port)'."
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

(define-record-type <vec3>
  (make-vec3 x y z)
  vec3?
  (x vec3-x)
  (y vec3-y)
  (z vec3-z))

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

(define (vec3+ . vecs)
  "Return the sum of VECS, as in vector space addition."
  (define (add u v)
    (vec3-bind (((x1 y1 z1) u) ((x2 y2 z2) v))
      (make-vec3 (+ x1 x2) (+ y1 y2) (+ z1 z2))))
  (if (zero? (length vecs))
      (make-vec3 0 0 0)
      (reduce add (cdr vecs) (car vecs))))

(define (vec3- . vecs)
  "Return the difference of VECS, as in vector space subtraction."
  (define (sub u v)
    (vec3-bind (((x1 y1 z1) u) ((x2 y2 z2) v))
      (make-vec3 (- x1 x2) (- y1 y2) (- z1 z2))))
  (if (zero? (length vecs))
      (make-vec3 0 0 0)
      (reduce sub (cdr vecs) (car vecs))))

(define (vec3* c u)
  "Return the vector U scaled by a constant C, as in vector space scalar
multiplication."
  (vec3-bind (((x y z) u))
    (make-vec3 (* c x) (* c y) (* c z))))

(define (vec3->string u)
  "Return a string representation of the vector U."
  (vec3-bind (((x y z) u))
    (parameterize
        ((current-output-port
          (open-output-string)))
      (display "<")
      (display x)
      (display ", ")
      (display y)
      (display ", ")
      (display z)
      (display ">")
      (get-output-string (current-output-port)))))

(define (vec3->list u)
  "Return a list (x y z) of the components of vector U."
  (vec3-bind (((x y z) u))
    (list x y z)))

(define (vec3-magnitude u)
  "Return the magnitude of vector U."
  (vec3-bind (((x y z) u))
    (sqrt (+ (square x) (square y) (square z)))))

(define (vec3-normalize u)
  "Return the normal vector parallel to vector U."
  (vec3* (/ 1 (vec3-magnitude u)) u))



;; Time-variant position.
(define-record-type <ray>
  (make-ray origin direction)
  ray?
  (origin    ray-origin)
  (direction ray-direction))

;; Arbitrarily-chosen constants for rendering.

(define image-width  1920)
(define image-height 1080)
(define image-aspect-ratio (/ image-width image-height))

(define viewport-height 2.0)
(define viewport-width (* viewport-height image-aspect-ratio))

(define camera-position (make-vec3 0 0 0))
(define focal-length 1.0)

(define (coordinate->ray x y)
  "Return the ray towards the point on the viewport plane corresponding to X, Y
in image space."
  (let* ((u (/ x (- image-width  1)))
         (v (/ y (- image-height 1)))
         (horizontal (make-vec3 viewport-width 0 0))
         (vertical   (make-vec3 0 viewport-height 0))
         (corner (vec3- camera-position
                        (vec3* 0.5 horizontal)
                        (vec3* 0.5 vertical)
                        (make-vec3 0 0 focal-length))))
    (make-ray camera-position (vec3+ corner
                                     (vec3* u horizontal)
                                     (vec3* v vertical)
                                     (vec3* (- 1) camera-position)))))




(define (lerp a b t)
  "Interpolate between A and B with parameter T."
  (+ (* (- 1.0 t) a) (* t b)))

(define (ray-color r)
  "Return an arbitrary color for R."
  (vec3-bind (((x y z) (vec3-normalize (ray-direction r))))
    (let ((t (* 0.5 (+ y 1.0))))
      (list (inexact->exact (round (* 255 (lerp 1.0 0.5 t))))
            (inexact->exact (round (* 255 (lerp 1.0 0.7 t))))
            (inexact->exact (round (* 255 (lerp 1.0 1.0 t))))))))

(let ((image (make-vector (* image-width image-height) '(0 0 0))))
  (define (coordinate->index x y)
    (+ x (* y image-width)))
  (let loop1 ((x 0))
    (let loop2 ((y 0))
      (unless (>= y (- image-height 1))
        (vector-set! image (coordinate->index x y) (ray-color (coordinate->ray x y)))
        (loop2 (+ y 1))))
    (unless (>= x (- image-width 1))
      (loop1 (+ x 1))))
  (write-ppm image-width image-height image))
