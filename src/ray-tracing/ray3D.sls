#!r6rs

(library (ray-tracing ray3D)
  (export ray3D make-ray3D ray3D?
          origin-of direction-of
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
    (fields (immutable origin origin-of)
            (immutable direction direction-of))
    (protocol
     (lambda (ctor)
       (lambda (org dir)
         (ctor org dir)))))


  (define (ray3D=? a b)
    (and
     (point3D=? (origin-of a) (origin-of b))
     (point3D=? (direction-of a) (direction-of b))))


  (define (ray3D-copy source)
    (make-ray3D (point3D-copy (origin-of source))
                (point3D-copy (direction-of source))))


  (define (ray3D-length ray)
    (sqrt
     (+ 
      (square (- (x-of (direction-of ray)) (x-of (origin-of ray))))
      (square (- (y-of (direction-of ray)) (y-of (origin-of ray))))
      (square (- (z-of (direction-of ray)) (z-of (origin-of ray)))))))


  (define (ray3D-length-from-camera ray)
    (sqrt
     (+ 
      (square (x-of (direction-of ray)))
      (square (y-of (direction-of ray)))
      (square (z-of (direction-of ray))))))


  (define (ray3D-point-at ray t)
    (point3D-add (origin-of ray)
                 (point3D-scalar-multiply (direction-of ray) t)))


  (define (ray3D-point-from-camera-at ray t)
    (point3D-scalar-multiply (direction-of ray) t))


  (define (ray3D-unit-ray ray)
    (make-ray3D (origin-of ray)
                (point3D-scalar-divide (point3D-subtract
                                        (direction-of ray)
                                        (origin-of ray))
                                       (ray3D-length ray))))


  (define (ray3D-unit-ray-from-camera ray)
    (make-ray3D camera-focal-point
                (point3D-scalar-divide (direction-of ray)
                                       (ray3D-length-from-camera ray)))))
