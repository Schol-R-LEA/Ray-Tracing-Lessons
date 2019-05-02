#!r6rs 

(import 
  (rnrs base (6))
  ;; composite standard library, imports most std libs
  (rnrs (6))
  ;; basic string formatting
  (srfi :28)
  ;; time/date functions
  (srfi :19)
  (ray-tracing point3D)
  (ray-tracing ray3D)
  (colors rgb-colors)
  (image-file-types ppm))

(define (trace-image nx ny color-range)
  (let ((ppm (make-ppm-record (open-output-file
                               (date->string (current-date 0) "background-~5.ppm"))
                              (exact (ceiling nx))
                              (exact (ceiling ny))
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
        (let* ((u (* i nx))
               (v (* j ny))
               (ray (make-ray3D camera-focal-point
                                (point3D-add lower-left-corner
                                             (point3D-add
                                              (point3D-scalar-multiply horizontal u)
                                              (point3D-scalar-multiply vertical v)))))
               (hue (compute-color ray color-range 0.5 0.7 1.0))))))
    (write-ppm ppm)))


(define (compute-color ray color-range r-scale g-scale b-scale)
  (let* ((unit-ray (ray3D-unit-ray-from-camera ray))
         (t (+ 1 (* 0.5
                    (y-of (direction-get unit-ray)))))
         (t-prime (- 1.0 t)))
    (add-colors
     (make-rgb-color t-prime t-prime t-prime)
     (scale-color (make-rgb-color r-scale g-scale b-scale)
                  (* t color-range)))))


(trace-image 200 100 255)
