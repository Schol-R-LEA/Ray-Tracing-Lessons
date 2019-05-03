#!r6rs

(library (ray-tracing ray3D)
  (export ray3D make-ray3D ray3D?
          origin-get direction-get
          ray3D=? ray3D-copy
          ray3D-length ray3D-length-from-camera
          ray3D-point-at ray3D-point-from-camera-at
          camera-focal-point default-unit-direction
          ray3D-unit-ray ray3D-unit-ray-from-camera)
  (import (rnrs base (6))
          (rnrs (6))
          (ray-tracing point3D))


  (define camera-focal-point (make-point3D 0 0 0))

  (define default-unit-direction (make-point3D 1.0 1.0 1.0))


  (define (square x) (* x x))

  
  (define-record-type (ray3D make-ray3D ray3D?)
    (fields (immutable origin origin-get)
            (immutable direction direction-get))
    (protocol
     (lambda (ctor)
       (lambda (org dir)
         (ctor org dir)))))


  (define (ray3D=? a b)
    (and
     (point3D=? (origin-get a) (origin-get b))
     (point3D=? (direction-get a) (direction-get b))))


  (define (ray3D-copy source)
    (make-ray3D (point3D-copy (origin-get source))
                (point3D-copy (direction-get source))))


  (define (ray3D-length ray)
    (sqrt
     (+ 
      (square (- (x-of (direction-get ray)) (x-of (origin-get ray))))
      (square (- (y-of (direction-get ray)) (y-of (origin-get ray))))
      (square (- (z-of (direction-get ray)) (z-of (origin-get ray)))))))


  (define (ray3D-length-from-camera ray)
    (sqrt
     (+ 
      (square (x-of (direction-get ray)))
      (square (y-of (direction-get ray)))
      (square (z-of (direction-get ray))))))


  (define (ray3D-point-at ray t)
    (point3D-add (origin-get ray)
                 (point3D-scalar-multiply (direction-get ray) t)))


  (define (ray3D-point-from-camera-at ray t)
    (point3D-scalar-multiply (direction-get ray) t))


  (define (ray3D-unit-ray ray)
    (make-ray3D (origin-get ray)
                (point3D-scalar-divide (point3D-subtract
                                        (direction-get ray)
                                        (origin-get ray))
                                       (ray3D-length ray))))


  (define (ray3D-unit-ray-from-camera ray)
    (make-ray3D camera-focal-point
                (point3D-scalar-divide (direction-get ray)
                                       (ray3D-length-from-camera ray)))))
