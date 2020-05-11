;; Packing procedures which depend either on
;; (rnrs arithmetic bitwise (6)), in the case of R6RS, or
;; srfi-151, in the case of R7RS.

(define (encode-u64 n)
  (list (arithmetic-shift (bitwise-and n #x00000000000000ff)   0)
        (arithmetic-shift (bitwise-and n #x000000000000ff00)  -8)
        (arithmetic-shift (bitwise-and n #x0000000000ff0000) -16)
        (arithmetic-shift (bitwise-and n #x00000000ff000000) -24)
        (arithmetic-shift (bitwise-and n #x000000ff00000000) -32)
        (arithmetic-shift (bitwise-and n #x0000ff0000000000) -40)
        (arithmetic-shift (bitwise-and n #x00ff000000000000) -48)
        (arithmetic-shift (bitwise-and n #xff00000000000000) -56)))

(define (encode-u32 n)
  (list (arithmetic-shift (bitwise-and n #x000000ff)   0)
        (arithmetic-shift (bitwise-and n #x0000ff00)  -8)
        (arithmetic-shift (bitwise-and n #x00ff0000) -16)
        (arithmetic-shift (bitwise-and n #xff000000) -24)))

(define (encode-u16 n)
  (list (arithmetic-shift (bitwise-and n #x00ff)  0)
        (arithmetic-shift (bitwise-and n #xff00) -8)))

(define (encode-u8 n) (list (bitwise-and n #xff)))



(define bmp-skeleton-size 48)

;; Binary output, which I can't get uniform across standards.

;; R7RS (Chicken).

(define-record-type <bmp>
  (bmp width height pixels)
  bmp?
  (width  bmp-width)
  (height bmp-height)
  (pixels bmp-pixels))

(define (set-pixel! bmp x y r g b)
  (let* ((index (+ x (* (bmp-width bmp) y)))
         (pixel (make-vector 3)))
    (vector-set! pixel 2 r)
    (vector-set! pixel 1 g)
    (vector-set! pixel 0 b)
    (vector-set! (bmp-pixels bmp) index pixel)))

(define (write-bytes bytes) (for-each write-byte bytes))
(define (write-bmp dest bmp)
  (with-output-to-file dest
    (lambda ()
      (let* ((pixels (bmp-pixels bmp))
             (image-size (* 3 (vector-length pixels)))
             (file-size (+ bmp-skeleton-size image-size)))
        ;; File header.
        (write-bytes '(66 77))
        (write-bytes (encode-u64 file-size))
        (write-bytes (encode-u32 #x36)) ;; Data start (constant).

        ;; Bitmap header.
        (write-bytes (encode-u32 #x28)) ;; Header size (constant).
        (write-bytes (encode-u32 (bmp-width bmp)))
        (write-bytes (encode-u32 (bmp-height bmp)))
        (write-bytes (encode-u16 1))    ;; nb plan (constant).
        (write-bytes (encode-u16 24))   ;; bpp (constant).
        (write-bytes (encode-u32 0))    ;; Uncompressed (constant).
        (write-bytes (encode-u32 image-size))
        (write-bytes '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) ;; Padding.

        ;; Pixel data.
        (let loop ((i 0))
          (unless (>= i (vector-length pixels))
            (write-byte (vector-ref (vector-ref pixels i) 0))  
            (write-byte (vector-ref (vector-ref pixels i) 1))  
            (write-byte (vector-ref (vector-ref pixels i) 2))
            (loop (+ i 1))))
        (write-bytes '(0 0 0))))))      ;; Padding.

(define (make-bmp width height)
  (bmp width height (make-vector (* width height) #(0 0 0))))

(let ((bmp (make-bmp 64 64)))
  (let loop ((x 0))
    (when (< x 64)
      (let inner ((y 0))
        (when (< y 64)
          (set-pixel! bmp x y y x y)
          (inner (+ y 1))))
      (loop (+ x 1))))
  (write-bmp "test.bmp" bmp))
