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

(local unpack (or _G.unpack table.unpack)) ; compatibility with newer luas

(fn map [f sequence]
  (var result [])
  (when sequence
    (for [i 1 (# sequence)]
      (tset result i (f (. sequence i)))))
  result)

(fn fold [f init sequence]
  (var result init)
  (when sequence
    (for [i 1 (# sequence)]
      (set result (f result (. sequence i)))))
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

(fn vec3->string [v]
  (let [{:x x :y y :z z} v]
    (string.format "<%f, %f, %f>" x y z)))

(fn vec3+ [...]
  "Return the sum of VECS, as in vector space addition."
  (fold (fn [a b]
          (let [{:x α :y β :z γ} a
                {:x x :y y :z z} b]
            (vec3 (+ α x) (+ β y) (+ γ z))))
        (vec3 0.00 0.00 0.00)
        [...]))

(fn vec3- [...]
  "Return the difference of VECS, as in vector space subtraction."
  (var seq [...])
  (if (= 0 (# seq))
      (vec3 0.00 0.00 0.00)
      (let [init (table.remove seq 1)]
        (fold (fn [a b]
                (let [{:x α :y β :z γ} a
                      {:x x :y y :z z} b]
                  (vec3 (- α x) (- β y) (- γ z))))
              init
              seq))))

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
  (* d (/ math.pi 180)))

(local image-width  1920)
(local image-height 1080)
(local image-aspect-ratio (/ image-width image-height))

(local camera-position (vec3  8.00  5.00  9.00))
(local camera-target   (vec3  0.25  0.00  0.50))
(local camera-up       (vec3  0.00  1.00  0.00))
(local camera-fov      30)

(fn coordinate->ray [x y]
  "Return the ray corresponding to the point X, Y on the viewport plane."
  (let [dist 1.0
        top    (* dist (math.tan (/ (degrees->radians camera-fov) 2)))
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
;;; Shapes and generic procedures for working with them.
;;;

(fn intersect-plane [r shape t-min t-max]
  (let [{ :n normal :p0 p0 } shape
        { :origin origin :direction direction } r
        normal (vec3-normalize normal)
        denominator (vec3-dot direction normal)]
    (if (~= 0 denominator)
        (let [t (/ (vec3-dot (vec3- p0 origin) normal)
                   denominator)]
          (if (<= t-min t t-max) t)))))

(fn plane [p0 n material]
  {
   :n n
   :p0 p0
   :material material

   :intersect intersect-plane
   :normal (fn [] (vec3-normalize n))
  })

(fn intersect-sphere [r shape t-min t-max]
  (let [{ :origin origin :direction direction } r
        { :center center :radius radius } shape
        oc (vec3- origin center)
        A  (vec3-dot direction direction)
        B  (* 2.0 (vec3-dot oc direction))
        C  (- (vec3-dot oc oc) (square radius))

        discriminant (- (square B) (* 4 A C))
        t (if (< 0 discriminant)
              (let [p (/ (+ (- B) (math.sqrt discriminant)) (* 2 A))
                    m (/ (- (- B) (math.sqrt discriminant)) (* 2 A))]
                (if (>= m t-min) m p))
              (/ (- B) (* 2 A)))]
    (if (and (<= 0 discriminant) (<= t-min t t-max)) t)))

(fn sphere [center radius material]
  {
   :center center
   :radius radius
   :material material

   :intersect intersect-sphere
   :normal (fn [position] (vec3-normalize (vec3- position center)))
  })


;;;
;;; Lights, and generic procedures for working with them.
;;;

(fn light-sample [intensity position direction]
  {
   :intensity intensity
   :position position
   :direction direction
   })

(fn spot-light-at [light point]
  (let [{ :from position :to target
          :intensity intensity :exponent exponent
          :cutoff-angle cutoff} light
        direction (vec3- position point)
        pf (vec3-normalize (vec3- point position))
        intensity (if (< (vec3-dot pf (vec3-normalize (vec3- target position)))
                         (math.cos (degrees->radians cutoff)))
                      (vec3 0.00 0.00 0.00)
                      (vec3* (* (/ 1 (square (vec3-magnitude direction)))
                                (math.pow (vec3-dot pf (vec3-normalize (vec3- target position)))
                                      exponent))
                             intensity))]
    (light-sample intensity position (vec3-normalize direction))))

(fn spot-light [from to intensity exponent cutoff-angle]
  {
   :from from
   :to to
   :intensity intensity
   :exponent exponent
   :cutoff-angle cutoff-angle

   :sample-at spot-light-at
  })


;;;
;;; Materials.
;;;

(fn diffuse-material [ka kd]      { :ka ka :kd kd})
(fn phong-material   [ka kd ks p] { :ka ka :kd kd :ks ks :p p})

(fn reflect [l n]
  "Compute reflected vector, by mirroring l around n."
  (vec3- (vec3* (* 2.00 (vec3-dot n l)) n) l))


(local ambient-light (vec3 0.01 0.01 0.01))
(local lights [(spot-light (vec3  10.00  10.00   5.00)
                           (vec3   0.00   0.00   0.00)
                           (vec3 100.00  96.00  88.00)
                           50
                           15)])
(local shapes [(sphere (vec3 -0.25 0.00 0.25)
                       1.25
                       (phong-material (vec3 1.0 0.2 0.2)
                                       (vec3 1.0 0.2 0.2)
                                       (vec3 2.0 2.0 2.0)
                                       20))
               (plane (vec3  0.00 -1.25  0.00)
                      (vec3  0.00  1.00  0.00)
                      (diffuse-material (vec3 1.0 1.0 0.2)
                                        (vec3 1.0 1.0 0.2)))])

(fn shade-pixel [shape position origin]
  (let [{:ka ka :kd kd :ks ks :p p} shape.material
        normal (shape.normal position)
        Ia (let [{:x α :y β :z γ} ka
                 {:x x :y y :z z} ambient-light]
             (vec3 (* α x) (* β y) (* γ z)))
        Id (vec3+
            (unpack
             (map (fn [light]
                    (let [sample (light.sample-at light position)
                          { :direction direction :intensity intensity } sample
                          scalar (math.max (vec3-dot normal direction) 0)
                          {:x α :y β :z γ} kd
                          {:x x :y y :z z} intensity]
                      (vec3 (* α x scalar)
                            (* β y scalar)
                            (* γ z scalar))))
                  lights)))
        Is (if ks
               (vec3+
                (unpack
                 (map (fn [light]
                        (let [sample (light.sample-at light position)
                              { :position point :intensity intensity } sample
                              l (vec3-normalize (vec3- point position))
                              v (vec3-normalize (vec3- origin position))
                              r (reflect l normal)
                              scalar (math.pow (math.max 0 (vec3-dot v r)) p)
                              {:x α :y β :z γ} ks
                              {:x x :y y :z z} intensity]
                          (vec3 (* α x scalar)
                                (* β y scalar)
                                (* γ z scalar))))
                      lights)))
               (vec3 0.00 0.00 0.00))
        {:x x :y y :z z} (vec3+ Ia Id Is)]
    [(math.floor (* 255 (math.min 1.0 x)))
     (math.floor (* 255 (math.min 1.0 y)))
     (math.floor (* 255 (math.min 1.0 z)))]))

(fn ray-intersect-scene [r]
  "Return the nearest shape with which RAY intersects, with the point, if any.
Otherwise, return nil."
  (match (fold (fn [a b]
                 (if (= a :none) b
                     (= b :none) a
                     (let [{ :intersection t1} a
                           { :intersection t2} b]
                       (if (> t1 t2) b a))))
               :none
               (map (fn [shape]
                      (let [intersect (. shape "intersect")
                            intersection (intersect r shape 0.001 10000)]
                        (if intersection
                            { :shape shape :intersection intersection }
                            :none)))
                    shapes))
    { :shape shape :intersection t } { :shape shape :point (point-at r t)}))


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
      (let [r (coordinate->ray (screen->viewport x y))
            intersection (ray-intersect-scene r)]
        (if intersection
            (let [{ :shape shape :point point } intersection]
              (set-pixel output x y (shade-pixel shape point camera-position)))
            (set-pixel output x y (ray-color r))))))
  (write-ppm output))

(main)
