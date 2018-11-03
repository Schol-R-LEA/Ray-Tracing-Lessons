#!r6rs

(library (ray-tracing types)
  (export )

  (import (rnrs base (6))
          ;; composite standard lbrary, imports most std libs
          (rnrs (6))
          (average))


  (define-condition-type &invalid-type-predicate-violation
    &syntax 
    make-type-predicate-violation
    invalid-type-predicate-violation?)


  (define-condition-type &invalid-transcoder-violation
    &syntax 
    make-invalid-transcoder-violation
    invalid-transcoder-violation?)

  (define-condition-type &invalid-accessor-code-violation
    &syntax 
    make-invalid-accessor-code-violation
    invalid-accessor-code-violation?)

  (define-condition-type &type-constraint-violation
    &syntax 
    make-quaternion-type-constraint-violation 
    quaternion-type-constraint-violation?)


  (define bytevector-accessors 
    (make-enumeration 
     '(u8 s8 u16 s16 u32 s32 u64 s64 uint sint float double)))

  (define accessor-lookup-table
    '((u8 bytevector-u8-ref)
      (s8  bytevector-s8-ref)
      (u16 bytevector-u16-ref)
      (s16  bytevector-s16-ref)
      (u32 bytevector-u32-ref)
      (s32  bytevector-s32-ref)
      (u64 bytevector-u64-ref)
      (s64  bytevector-s64-ref)
      (uint bytevector-uint-ref)
      (sint  bytevector-sint-ref)
      (float bytevector-ieee-single-ref)
      (double bytevector-ieee-double-ref)))


  

  #| 
  A type that represents an abstract quaternion as a bytevector. 
  |#
  (define quaternion 
    (make-record-type-descriptor 
     'quaternion
     #f #f #f #f 
     '#((immutable coordinate-elements)
        (immutable bytevector-accessor))))


  (define make-quaternion
    (make-record-constructor-descriptor 
     'quaternion #f
     (lambda (ctor)
       (lambda (type-predicate u8-transcoder accessor-code x y z w)
         (cond ((not ((procedure? type-predicate))) 
                (raise &invalid-type-predicate-violation))
               ((not (procedure? u8-transcoder))
                (raise &invalid-transcoder-violation))
               ((not (assoc accessor-code accessor-lookup-table))
                (raise &invalid-accessor-code-violation))
               ((not (and 
                      (type-predicate x) 
                      (type-predicate y)
                      (type-predicate z)
                      (type-predicate w)))
                (raise &type-constraint-violation))
               (else 
                (let ((xyzw-list 
                       (append 
                        (u8-transcoder x)
                        (u8-transcoder y)
                        (u8-transcoder z)
                        (u8-transcoder w))))
                  (ctor 
                   (u8-list->bytevector xyzw-list)
                   (cdar (assoc accessor-code accessor-lookup-table))))))))))

  (define quaternion?
    (record-predicate 'quaternion)))

(define (x-of-qua q) 
  )


(define-record-type-descriptor vector-3D
  quaternion #f #f #f ))





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
