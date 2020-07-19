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

(fn pack [...]
  (var result [])
  (let [n (select "#" ...)]
    (for [i 1 n]
      (tset result i (select i ...))))
  result)

(fn fold [f init sequence]
  (var result init)
  (for [i 1 (# sequence)]
    (set result (f result (. sequence i))))
  result)


;;;
;;; Image encoding.
;;;

(fn write-ppm [image]
  "Encode the WIDTH by HEIGHT image given as PIXELS into the portable pixmap
format (PPM), writing the result to standard output."
  (print "P3")
  (print (string.format "%d %d" image.width image.height))
  (print "255")
  (for [i 1 (# image.pixels)]
    (let [[r g b] (. image.pixels i)]
      (print (string.format "%d %d %d" r g b)))))

(fn set-pixel [image x y color]
  "Set the color of the pixel at X, Y to COLOR."
  (let [offset (+ 1 x (* y image.width))]
    (tset image.pixels offset color)))

(fn image [width height]
  "Return an image object with dimensions WIDTH by HEIGHT."
  (let [pixels {}]
    (for [i 1 (* width height)]
      (tset pixels i [0 0 0]))
    {:width width :height height :pixels pixels}))


;;;
;;; Are mathematical objects in the room with us right now?
;;;

(fn square [n] (* n n))

(fn vec3 [x y z] {:x x :y y :z z})

(fn vec3+ [...]
  "Return the sum of VECS, as in vector space addition."
  (fold (fn [a b]
          (let [{:x α :y β :z γ} a
                {:x x :y y :z z} b]
            (vec3 (+ α x) (+ β y) (+ γ z))))
        (vec3 0.00 0.00 0.00)
        (pack ...)))

(fn vec3- [...]
  "Return the difference of VECS, as in vector space subtraction."
  (fold (fn [a b]
          (let [{:x α :y β :z γ} a
                {:x x :y y :z z} b]
            (vec3 (- α x) (- β y) (- γ z))))
        (vec3 0.00 0.00 0.00)
        (pack ...)))

(fn vec3* [c u]
  "Return the vector U scaled by a constant C, as in vector space scalar
multiplication."
  (let [{:x x :y y :z z} u]
    (vec3 (* c x) (* c y) (* c z))))

(fn vec3-dot [u v]
  "Return the dot product of the vectors U and V."
  (let [{:x α :y β :z γ} u
        {:x x :y y :z z} v]
    (+ (* α x) (* β y) (* γ z))))

(fn vec3-cross [u v]
  "Return the cross product of the vectors U and V."
  (let [{:x α :y β :z γ} u
        {:x x :y y :z z} v]
    (vec3 (- (* β z) (* γ y))
          (- (* γ x) (* α z))
          (- (* α y) (* β x)))))

(fn vec3-magnitude [u]
  "Return the magnitude of vector U."
  (let [{:x x :y y :z z} u]
    (math.sqrt (+ (square x) (square y) (square z)))))

(fn vec3-normalize [u]
  "Return the normal vector parallel to vector U."
  (vec3* (/ 1 (vec3-magnitude u)) u))

(fn ray [origin direction] { :origin origin :direction direction })

(fn point-at [r t]
  "Return the position of RAY at time T."
  (let [{ :origin origin :direction direction } r]
    (vec3+ origin (vec3* t direction))))

(fn degrees->radians [d]
  "Convert D, a value in degrees, to radians."
  (* d (/ math.pi 360)))

(global image-width  1920)
(global image-height 1080)
(global image-aspect-ratio (/ image-width image-height))

(global camera-position (vec3  8.00  5.00  9.00))
(global camera-target   (vec3  0.25  0.00  0.50))
(global camera-up       (vec3  0.00  1.00  0.00))
(global camera-fov      30)

(fn coordinate->ray [x y]
  "Return the ray corresponding to the point X, Y on the viewport plane."
  (let [dist 1.0
        top    (* dist (math.tan (degrees->radians camera-fov)))
        right  (* top image-aspect-ratio)
        bottom (- top)
        left   (- right)
        W (vec3-normalize (vec3- camera-position camera-target))
        U (vec3-normalize (vec3-cross camera-up W))
        V (vec3-cross W U)
        corner (vec3+ camera-position
                      (vec3* left     U)
                      (vec3* bottom   V)
                      (vec3* (- dist) W))
        across (vec3* (* 2 right) U)
        up     (vec3* (* 2 top)   V)]
    (ray camera-position
         (vec3-normalize
          (vec3+ corner
                 (vec3* x across)
                 (vec3* y up)
                 (vec3* (- 1) camera-position))))))


;;;
;;; Scene graph.
;;;

(fn lerp [a b t]
  "Interpolate between A and B with parameter T."
  (+ (* (- 1.0 t) a) (* t b)))

(fn ray-color [r]
  "Return an arbitrary color for R."
  (let [{:direction direction} r
        {:y y} (vec3-normalize direction)
        t (* 0.5 (+ y 1.0))]
    [(math.floor (* 255 (lerp 1.0 0.5 t)))
     (math.floor (* 255 (lerp 1.0 0.7 t)))
     (math.floor (* 255 (lerp 1.0 1.0 t)))]))

(fn screen->viewport [x y]
  (values (/ x image-width)
          (/ (- image-height 1 y)
             image-height)))

(fn main []
  (var output (image image-width image-height))
  (for [x 0 image-width]
    (for [y 0 image-height]
      (let [r (coordinate->ray (screen->viewport x y))]
        (set-pixel output x y (ray-color r)))))
  (write-ppm output))

(main)
