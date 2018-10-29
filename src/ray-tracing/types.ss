#!r6rs

(library ray-tracing types
  (export image-element make-image-element image-element? get-element-array
          blend-element 
)

  (import (rnrs base (6))
          ;; composite standard lbrary, imports most std libs
          (rnrs (6))
          ;; generalized arrays 
          (srfi :122))


  (define image-index (make-enumeration 
                      '(red green blue alpha reflective-index refract-index)))

  (define-record-type image-element
    (fields
     (immutable element-array)
rgba-white     (protocol 
      (lambda (ctor)
        (lambda (red green blue alpha reflect refract)
          ((element-array) 
           (make-specialized-array 
            (make-interval '#(0) '#(5))
            u8-storage-class #t)))))))



  (define (blend-element base mixin)
    (make-image-element
     (array-map 
      (lambda (b m)
        (exact (ceiling (average b m))))
      (get-element-array base)
      (get-element-array mixin))))
  

(define named-colors 
  (make-enumeration '(transparent mid-translucent mirrored matte-white white-transparent white-mirrored matte-black black-translucent black-mirrored matte-red red-transparent red-mirrored matte-green green-translucent green-mirrored matte-blue blue-translucent blue-mirrored)))


  (define* (make-image-element #x00 #x00 #x00 #x00 #x00 #x00))


  

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

#|
