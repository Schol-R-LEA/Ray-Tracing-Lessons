#!r6rs 

(import 
  (rnrs base (6))
  ;; composite standard library, imports most std libs
  (rnrs (6))
  ;; intermediate string formatting
  (srfi :28)
  ;; time/date functions
  (srfi :19)
  (ray-tracing point3D)
  (ray-tracing ray3D)
  (colors rgb-colors)
  (image-file-types ppm))


(define (square x) (* x x))


(define (format-point3D vec)
  (format "<~A,~A,~A>" (x-of vec) (y-of vec) (z-of vec)))


(define (format-ray3D ray)
  (format "{~A,~A}~%"
          (format-point3D (origin-of ray))
          (format-point3D (direction-of ray))))


(define (trace-image nx ny color-range)
  (let ((ppm (make-ppm-record (open-output-file
                               (date->string (current-date 0) "shaded-sphere-~5.ppm"))
                              (exact (floor nx))
                              (exact (floor ny))
                              (+ color-range 0.99)
                              ""))
        (lower-left-corner (make-point3D -2.0 -1.0 -1.0))
        (horizontal (make-point3D 4.0 0.0 0.0))
        (vertical (make-point3D 0.0 2.0 0.0)))
    (do ((j (- (ppm-height ppm) 1)
            (- j 1)))
        ((< j 0))
      (do ((i 0
              (+ i 1)))
          ((>= i (ppm-width ppm)))
        (let* ((u (/ i nx))
               (v (/ j ny))
               (ray (current-path lower-left-corner horizontal vertical u v))
               (hue (color ray (ppm-colors ppm) 0.5 0.7 1.0)))
          (set! ppm (ppm-append-pixel ppm hue))))
      (set! ppm (ppm-terminate-row ppm)))
    (write-ppm ppm)))


(define (color ray color-range r g b)
  (let* ((offset (make-point3D 0 0 -1))
         (surface (hit-sphere offset 0.5 ray)))
    (if (> surface 0.0)
        (let* ((at (ray3D-point-at ray surface))
               (offset-from-point-at (make-ray3D
                                      camera-focal-point
                                      (point3D-subtract at offset)))
               (unit-ray-to (ray3D-unit-ray offset-from-point-at))
               (normal
                (point3D-scalar-add (direction-of unit-ray-to) 1.0)))
          (display (format "~A * ~A == <~A,~A,~A>~%"
                           (format-point3D normal)
                           0.5
                           (* 0.5 (x-of normal)) (* 0.5 (y-of normal)) (* 0.5 (z-of normal))))
          (make-rgb-color (* color-range 0.5 (x-of normal))
                          (* color-range 0.5 (y-of normal))
                          (* color-range 0.5 (z-of normal))))
        (let* ((unit-ray (ray3D-unit-ray-from-camera ray))
               (t (* 0.5
                     (+ 1.0
                        (y-of (direction-of unit-ray)))))
               (t-prime (- 1.0 t)))
          (make-rgb-color (* (+ t-prime (* t r)) color-range)
                          (* (+ t-prime (* t g)) color-range)
                          (* (+ t-prime (* t b)) color-range))))))


(define (hit-sphere sphere-center radius ray)
  (let* ((center-offset (point3D-subtract (origin-of ray) sphere-center))
         (direction (direction-of ray))
         (a (point3D-dot-product direction direction))
         (b (* (point3D-dot-product center-offset direction) 2.0))
         (c (- (point3D-dot-product center-offset center-offset) (square radius)))
         (discriminant (- (square b) (* 4 (* a c)))))
    (if (< discriminant 0)
        -1.0
        (/ (- (- b) (sqrt discriminant)) (* 2 a)))))


(define (current-path corner horizontal vertical u v)
  (make-ray3D camera-focal-point
              (point3D-add corner
                           (point3D-add
                            (point3D-scalar-multiply vertical v)
                            (point3D-scalar-multiply horizontal u)))))


(trace-image 800 400 255)
