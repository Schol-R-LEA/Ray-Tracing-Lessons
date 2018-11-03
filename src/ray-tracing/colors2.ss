#!r6rs

(library (ray-tracing types)
  (export rgb-color make-rgb-color rgb-color?
          red-of green-of blue-of color=?
          get-rgb-list color->numeric-string
          blend-colors add-colors subtract-colors
          standard-colors rgb-white rgb-black 
          rgb-red rgb-green rgb-blue
          rgb-yellow rgb-cyan rgb-magenta
          rgb-orange rgb-indigo rgb-maroon)

  (import (rnrs base (6))
          ;; composite standard lbrary, imports most std libs
          (rnrs (6))
          (ray-tracing vector3D)
          (average))

  (define (rgb-element? rgb-element)
    (and (integer? rgb-element)
         (< 0 rgb-element)
         (< rgb-element 256)))


  (define (rgb-transcode rgb-element)
    rgb-element)


(define (u8-constrain rgb-element)
  ())


  (define rgb-color 
    (make-record-type-descriptor 
     'rgb-color
     #| 
     A type that represents conventional 24-bit color
     as three 8-bit integers. 
     |#
    'vector3D #f #f #f
    '#()))


  (define make-rgb-color
    (make-record-constructor-descriptor
     rgb-color make-vector3D
     (lambda (ctor-rgb-color)
       (lambda (r g b)
         (ctor-vec3D rgb-element? rgb-transcoder field-accessor r g b)))))

  (define rgb-color?
    (record-predicate 'rgb-color))

  (define (red-of color)
    (vec3D-x-of color rgb-element? rgb-transcoder bytevector-u8-ref))

  (define (green-of color)
    (vec3D-y-of color rgb-element? rgb-transcoder bytevector-u8-ref))

  (define (blue-of color)
    (vec3D-x-of color rgb-element? rgb-transcoder bytevector-u8-ref))


  (define  (get-rgb-list color)
    (list (red-of color) (green-of color) (blue-of color)))

  (define (color->numeric-string color)
    (string-append ""
     (number->string (red-of color))
     " "
     (number->string (green-of color))
     " "
     (number->string (blue-of color))))


  (define (color=? base compared)
    (and
     (= (red-of base) (red-of compared))
     (= (green-of base) (green-of compared))
     (= (blue-of base) (blue-of compared))))
  
  
  (define (blend-colors base mixin)
    (make-rgb-color
     ((avg (red-of base) (red-of mixin)))
     ((avg (green-of base) (green-of mixin)))
     ((avg (blue-of base) (blue-of mixin)))))
  
  (define (add-colors base mixin)
    (make-rgb-color
     (+ (red-of base) (red-of mixin))
     (+ (green-of base) (green-of mixin))
     (+ (blue-of base) (blue-of mixin))))
  
  (define (subtract-colors base mixin)
    (make-rgb-color
     (abs (- (red-of base) (red-of mixin)))
     (abs (- (green-of base) (green-of mixin)))
     (abs (- (blue-of base) (blue-of mixin)))))   
  
  ;; black and white  
  (define rgb-white (make-rgb-color #xff #xff #xff))
  (define rgb-black (make-rgb-color #x0 #x0 #x0))
  ;; additive primary colors
  (define rgb-red (make-rgb-color #xff #x0 #x0))
  (define rgb-green (make-rgb-color #x0 #xff #x0))
  (define rgb-blue (make-rgb-color #x0 #x0 #xff))
  ;; subtractive primary colors
  (define rgb-yellow (make-rgb-color #x7f #x7f #x0))
  (define rgb-cyan (make-rgb-color #x0 #x7f #x7f))
  (define rgb-magenta (make-rgb-color #x7f #x0 #x7f))  
  ;; secondary additive colors
  (define rgb-orange (make-rgb-color #xff #xff #x0))
  (define rgb-indigo (make-rgb-color #x0 #xff #xff))
  (define rgb-maroon (make-rgb-color #xff #x0 #xff))    


  (define standard-colors 
    (list
     rgb-white rgb-black 
     rgb-red rgb-green rgb-blue
     rgb-yellow rgb-cyan rgb-magenta
     rgb-orange rgb-indigo rgb-maroon)))





#|

(define-record-type spacial-coordinate
(fields
(immutable )
(immutable rgba-color)))


(define-record-type focal-pixel
(fields
(immutable width x)
(immutable height y)
(immutable rgba-color)))


(define-record-type panel
(fields (immutable plane))
(protocol
(lambda (ctor)
(lambda (x y)
((plane) (make-specialized-array 
))))))



(define-record-type space
)

|#
