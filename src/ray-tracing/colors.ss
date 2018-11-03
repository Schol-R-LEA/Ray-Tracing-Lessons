#!r6rs

(library (ray-tracing colors)
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

  (define (u8-constrain rgb-element)
    (max 0 (min 255 (exact (ceil rgb-element)))))


  (define (rgb-transcode rgb-element)
    (u8-constrain rgb-element))


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
         (ctor-vec3D rgb-element? rgb-transcoder 
                     (u8-constrain r) 
                     (u8-constrain g)
                     (u8-constrain b))))))

  (define make-rgb-color-from-vec3D
    (make-record-constructor-descriptor
     rgb-color make-vector3D-from-bytevector
     (lambda (ctor-rgb-color)
       (lambda (vec)
         (ctor-vec3D/bv 
          integer? 
          'u8
          (make-vector3D (u8-constrain (vec3D-x-of vec bytevector-u8-ref))
                         (u8-constrain (vec3D-y-of vec bytevector-u8-ref))
                         (u8-constrain (vec3D-z-of vec bytevector-u8-ref))))))))
  
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
    (vec3=? base compared bytevector-u8-ref))
  
  
  (define (blend-colors base mixin)
    (make-rgb-color
     (u8-constrain (avg (red-of base)(red-of mixin)))
     (u8-constrain (avg (green-of base) (green-of mixin)))
     (u8-constrain (avg (blue-of base) (blue-of mixin)))))
    
  (define (add-colors base mixin)
    (make rgb-color-from-vec3D 
      (vec3D-add base mixin bytevector-u8-ref)))
  
  
  (define (sub-colors base mixin)
    (make rgb-color-from-vec3D 
      (vec3D-sub base mixin bytevector-u8-ref)))
  
      
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
