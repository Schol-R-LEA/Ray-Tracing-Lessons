
#!r6rs

(library (colors rgb-colors)
  (export make-rgb-color rgb-color?
          red-of green-of blue-of color=?
          rgb-color->list rgb-color->numeric-string
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
    (max 0 (min 255 (exact (floor rgb-element)))))


  (define (rgb-element? el)
    (integer? el))

  (define (rgb-transcode rgb-element)
    (list (u8-constrain rgb-element)))

  
  ;;;A type that represents conventional 24-bit color
  ;;;as three 8-bit integers. 


  (define rd-rgb-color
    (make-record-type-descriptor 
     'rgb-color
     rd-vector3D #f #f #f 
     '#()))


  (define cd-rgb-color 
    (make-record-constructor-descriptor
     rd-rgb-color cd-vector3D
     (lambda (ctor-vec3d)
       (lambda (r g b)
         (let ((ctor-rgb 
                (ctor-vec3d rgb-element?
                            rgb-transcode 
                            (u8-constrain r) 
                            (u8-constrain g)
                            (u8-constrain b))))
           (ctor-rgb))))))

  (define make-rgb-color 
    (record-constructor cd-rgb-color))

  (define rgb-color?
    (record-predicate rd-rgb-color))

  (define (red-of color)
    (vec3d-x-of color bytevector-u8-ref))

  (define (green-of color)
    (vec3d-y-of color bytevector-u8-ref))

  (define (blue-of color)
    (vec3d-z-of color bytevector-u8-ref))


  (define  (rgb-color->list color)
    (list (red-of color) (green-of color) (blue-of color)))

  (define (rgb-color->numeric-string color)
    (string-append ""
                   (number->string (red-of color))
                   " "
                   (number->string (green-of color))
                   " "
                   (number->string (blue-of color))))


  (define (color=? base compared)
    (vec3d=? base compared bytevector-u8-ref))


  (define (blend-colors base mixin)
    (make-rgb-color
     (u8-constrain (avg (red-of base) (red-of mixin)))
     (u8-constrain (avg (green-of base) (green-of mixin)))
     (u8-constrain (avg (blue-of base) (blue-of mixin)))))

  (define (add-colors base mixin)
    (make-rgb-color
     (u8-constrain 
      (+ (red-of base) (red-of mixin)))
     (u8-constrain 
      (+ (green-of base) (green-of mixin)))
     (u8-constrain 
      (+ (blue-of base) (blue-of mixin)))))

  (define (subtract-colors base mixin)
    (make-rgb-color
     (u8-constrain 
      (- (red-of base) (red-of mixin)))
     (u8-constrain 
      (- (green-of base) (green-of mixin)))
     (u8-constrain 
      (- (blue-of base) (blue-of mixin)))))


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
