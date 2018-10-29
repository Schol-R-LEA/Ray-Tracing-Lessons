#!r6rs

(library (ray-tracing types)
  (export rgb-color make-rgb-color rgb-color?
          red-of green-of blue-of colors-=? get-rgb-list
          blend-colors add-colors subtract-colors
          standard-colors rgb-white rgb-black 
          rgb-red rgb-green rgb-blue
          rgb-yellow rgb-cyan rgb-magenta
          rgb-orange rgb-indigo rgb-maroon
          avg)

  (import (rnrs base (6))
          ;; composite standard lbrary, imports most std libs
          (rnrs (6)))

  (define (avg n m . x)
    (/ 
     (+ n m
        (let sum ((y (if (null? x) 0 (car x)))
                  (rest (if (null? x) '() (cdr x))))
          (if (null? rest)
              y 
              (+ y (sum (car rest) (cdr rest))))))
     (+ 2 (length x))))


  (define-record-type rgb-color
    #| 
      A type that represents conventional 24-bit color
      as three 8-bit integers. 
    |#
    (fields (immutable red red-of) 
            (immutable green green-of) 
            (immutable blue blue-of))
    (protocol
     (lambda (ctor)
       (lambda (r g b)
         ;; saturate the values below 0 or above 255
         (ctor (min #xff (max 0 (exact (floor r))))
               (min #xff (max 0 (exact (floor g))))
               (min #xff (max 0 (exact (floor b)))))))))

  (define  (get-rgb-list color)
    (list (red-of color) (green-of color) (blue-of color)))

  (define (colors-=? base compared)
    (and
     (= (red-of base) (red-of compared))
     (= (green-of base) (green-of compared))
     (= (blue-of base) (blue-of compared))))
  
  
  (define (blend-colors base mixin)
    (make-rgb-color
     (avg (red-of base) (red-of mixin))
     (avg (green-of base) (green-of mixin))
     (avg (blue-of base) (blue-of mixin))))
  
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
