#!r6rs

(library (colors rgb-colors)
  (export make-rgb-color rgb-color?
          red-of green-of blue-of rgb-color=?
          rgb-color->list rgb-color->numeric-string
          blend-colors add-colors subtract-colors scale-color
          standard-colors rgb-white rgb-black 
          rgb-red rgb-green rgb-blue
          rgb-yellow rgb-cyan rgb-magenta
          rgb-orange rgb-indigo rgb-maroon)
  (import (rnrs base (6))
          ;; composite standard lbrary, imports most std libs
          (rnrs (6))
          (average))
  

  (define (u8-constrain rgb-element)
    (max 0 (min 255 (exact (floor rgb-element)))))


  (define (rgb-element? el)
    (integer? el))


  (define (rgb-transcode rgb-element)
    (list (u8-constrain rgb-element)))


  ;;;A type that represents conventional 24-bit color
  ;;;as three 8-bit integers. 
  (define-record-type rgb-color
    (fields
     (immutable red red-of)
     (immutable green green-of)
     (immutable blue blue-of))
    (protocol
     (lambda (ctor)
       (lambda (r g b)
         (ctor (u8-constrain r) 
               (u8-constrain g)
               (u8-constrain b))))))


  (define (rgb-color->list color)
    (list (red-of color) (green-of color) (blue-of color)))


  (define (rgb-color->numeric-string color)
    (string-append ""
                   (number->string (red-of color))
                   " "
                   (number->string (green-of color))
                   " "
                   (number->string (blue-of color))))


  (define (rgb-color=? base compared)
    (and
     (= (red-of base) (red-of compared))
     (= (green-of base) (green-of compared))
     (= (blue-of base) (blue-of compared))))


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


  (define (scale-color color scale-factor)
    (make-rgb-color
     (u8-constrain 
      (- (red-of color) scale-factor))
     (u8-constrain 
      (- (green-of color) scale-factor))
     (u8-constrain 
      (- (blue-of color) scale-factor))))


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
