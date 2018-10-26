#!r6rs

(library ray-tracing types
  (export spacial-coordinate pixel ray color)

  (import (rnrs base (6))
          ;; composite standard lbrary, imports most std libs
          (rnrs (6))
          ;; generalized arrays 
          (srfi :122))


  (define-record-type rgba-color
    (fields
     (immutable red r)
     (immutable green g)
     (immutable blue b)
     (immutable alpha a)))

  (define-record-type spacial-coordinate
    (fields
     (immutable width x)
     (immutable height y)
     (immutable depth z)
     (immutable rgb-color)))

  (define-record-type focal-pixel
    (fields
     (immutable width x)
     (immutable height y)
     (immutable rgb-color)))


  (define rgba-white (make-rgba-color #xff #xff #xff #xff))
  (define rgba-black (make-rgba-color #x0 #x0 #x0 #xff))
  (define rgba-red (make-rgba-color #xff #x0 #xff #xff))
  (define rgba-green (make-rgba-color #x0 #xff #x0 #xff))
  (define rgba-blue (make-rgba-color #x0 #x0 #xff #xff))
  (define rgba-transparent (make-rgba-color #xff #xff #xff #x0))
  (define rgba-translucent-black (make-rgba-color #x0 #x0 #x0 #x0))
  (define rgba-translucent-red (make-rgba-color #xff #x0 #xff #x0))
  (define rgba-translucent-green (make-rgba-color #x0 #xff #x0 #x0))
  (define rgba-translucent-blue (make-rgba-color #x0 #x0 #xff #x0))
  

  (define (rgba-blend color color-scaling mixin mixin-scaling)
    (make-rgba-color 
     (exact (ceiling (/ (+ (* (r color) color-scaling) (*  (r mixin) mixin-scaling) 2))))
     (exact (ceiling (/ (+ (* (g color) color-scaling) (*  (g mixin) mixin-scaling) 2))))
     (exact (ceiling (/ (+ (* (b color) color-scaling) (*  (b mixin) mixin-scaling) 2))))
     (exact (ceiling (/ (+ (* (a color) color-scaling) (*  (a mixin) mixin-scaling) 2))))))
)

