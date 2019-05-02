#!r6rs

(library (ray-tracing point3D)
  (export point3D make-point3D point3D?
          x-of y-of z-of
          point3D=? point3D-copy
          point3D-add point3D-subtract
          point3D-scalar-multiply point3D-flat-multiply
          point3D-scalar-divide point3D-flat-divide
          point3D-dot-product point3D-cross-product)
  (import (rnrs base (6))
          (rnrs (6)))


  (define-record-type (point3D make-point3D point3D?)
    (fields (immutable x x-of)
            (immutable y y-of)
            (immutable z z-of))
    (protocol
     (lambda (ctor)
       (lambda (a b c)
         (ctor a b c)))))


  (define (point3D=? a b)
    (and
     (point3D? a)
     (point3D? b)
     (= (x-of a) (x-of b))
     (= (y-of a) (y-of b))
     (= (z-of a) (z-of b))))


  (define (point3D-copy source)
    (make-point3D (x-of source) (y-of source) (z-of source)))


  (define (point3D-add augend addend)
    (make-point3D
     (+ (x-of augend) (x-of addend))
     (+ (y-of augend) (y-of addend))
     (+ (z-of augend) (z-of addend))))


  (define (point3D-subtract minuend subtrahend)
    (make-point3D
     (- (x-of minuend) (x-of subtrahend))
     (- (y-of minuend) (y-of subtrahend))
     (- (z-of minuend) (z-of subtrahend))))


  (define (point3D-scalar-multiply multiplier multiplicand)
    (make-point3D
     (* (x-of multiplier) multiplicand)
     (* (y-of multiplier) multiplicand)
     (* (z-of multiplier) multiplicand)))    
  

  (define (point3D-flat-multiply multiplier multiplicand)
    (make-point3D
     (* (x-of multiplier) (x-of multiplicand))
     (* (y-of multiplier) (y-of multiplicand))
     (* (z-of multiplier) (z-of multiplicand))))    


  (define (point3D-scalar-divide dividend divisor)
    (make-point3D
     (/ (x-of dividend) divisor)
     (/ (y-of dividend) divisor)
     (/ (z-of dividend) divisor)))    

  
  (define (point3D-flat-divide dividend divisor)
    (make-point3D
     (/ (x-of dividend) (x-of divisor))
     (/ (y-of dividend) (y-of divisor))
     (/ (z-of dividend) (z-of divisor))))    

  
  (define (point3D-dot-product multiplier multiplicand)
    (+ 
     (* (x-of multiplier) (x-of multiplicand))
     (* (y-of multiplier) (y-of multiplicand))
     (* (z-of multiplier) (z-of multiplicand))))


  (define (point3D-cross-product multiplier multiplicand)
    (make-point3D
     (- (* (y-of multiplier) (z-of multiplicand))
        (* (z-of multiplier) (y-of multiplicand)))
     (- (- (* (x-of multiplier) (z-of multiplicand))
           (* (z-of multiplier) (x-of multiplicand))))
     (- (* (x-of multiplier) (y-of multiplicand))
        (* (y-of multiplier) (x-of multiplicand))))))
