#!r6rs

(library (ray-tracing vector3D)
  (export vector3D make-vector3D vector3D?
          vector3D-get-bytevector vector3D-n-of
          vec3d-w-of vec3d-x-of vec3d-y-of vec3d-z-of         
          vector3D-get-list
          vec3d=? vec3d-scale vec3d-vector-sum
          vec3d-negate vec3d-add vec3d-sub vec3d-average
          vec3d-dot-product vec3d-cross-product

          bytevector-accessors

          &invalid-type-predicate-violation
          make-type-predicate-violation
          invalid-type-predicate-violation?
          &invalid-transcoder-violation
          make-invalid-transcoder-violation
          invalid-transcoder-violation?
          &invalid-accessor-code-violation
          make-invalid-accessor-code-violation
          invalid-accessor-code-violation?          
          &type-constraint-violation
          make-vector3D-type-constraint-violation 
          vector3D-type-constraint-violation?
)

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
    make-vector3D-type-constraint-violation 
    vector3D-type-constraint-violation?)


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
  A type that represents an abstract 3D vector as a bytevector. 
  |#
  (define vector3D 
    (make-record-type-descriptor 
     'vector3D
     #f #f #f #f 
     '#((immutable coordinate-elements))))


  (define make-vector3D
    (make-record-constructor-descriptor 
     'vector3D #f
     (lambda (ctor)
       (lambda (type-predicate u8-transcoder accessor-code x y z)
         (cond ((not ((procedure? type-predicate))) 
                (raise &invalid-type-predicate-violation))
               ((not (procedure? u8-transcoder))
                (raise &invalid-transcoder-violation))
               ((not (assoc accessor-code accessor-lookup-table))
                (raise &invalid-accessor-code-violation))
               ((not (and
                      (type-predicate x) 
                      (type-predicate y)
                      (type-predicate z)))
                (raise &type-constraint-violation))
               (else 
                (let ((xyzw-list 
                       (append 
                        (u8-transcoder x)
                        (u8-transcoder y)
                        (u8-transcoder z))))
                  (ctor 
                   (u8-list->bytevector xyz-list)))))))))

  (define vector3D?
    (record-predicate 'vector3D))

  (define vector3D-get-bytevector
    (record-accessor 'vector3D 0))

  (define (vector3D-n-of q n field-accessor)
    (field-accessor (vector3D-get-bytevector) n))

  (define (vec3d-x-of q field-accessor) 
    (vector3D-n-of q 0))

  (define (vec3d-y-of q field-accessor) 
    (vector3D-n-of q 1))

  (define (vec3d-z-of q field-accessor) 
    (vector3D-n-of q 2))


  (define  (vector3D-list q field-accessor)
    (list (vec3d-x-of q field-accessor)
          (vec3d-y-of q field-accessor)
          (vec3d-z-of q field-accessor)))


  (define (make-vec3d-unit-vector field-accessor type-predicate transcoder ) 
    (make-vector3d field-accessor type-predicate transcoder 1 0 0))


  (define (vec3d=? base compared)
    (and
     (= (vec3d-x-of base field-accessor) (vec3d-x-of compared field-accessor))
     (= (vec3d-y-of base field-accessor) (vec3d-y-of compared field-accessor))
     (= (vec3d-z-of base field-accessor) (vec3d-z-of compared field-accessor))))


  (define (vec3d-scale base scaling-factor field-accessor type-predicate transcoder)
    (make-vector3D
     type-predicate
     transcoder
     accessor-code
     (* (vec3d-x-of base field-accessor) scaling-factor)
     (* (vec3d-y-of base field-accessor) scaling-factor)
     (* (vec3d-z-of base field-accessor) scaling-factor)))

  (define (vec3d-vector-sum base addend field-accessor)
    (make-vector3D
     type-predicate
     transcoder
     accessor-code     
     (* (vec3d-x-of base field-accessor) addend)
     (* (vec3d-y-of base field-accessor) addend)
     (* (vec3d-z-of base field-accessor) addend)))


  (define (vec3d-add base addend field-accessor type-predicate transcoder field-accessor type-predicate transcoder)
    (make-vector3D
     type-predicate
     transcoder
     accessor-code)
     (+ (vec3d-x-of base field-accessor) (vec3d-x-of addend field-accessor))
     (+ (vec3d-y-of base field-accessor) (vec3d-y-of addend field-accessor))
     (+ (vec3d-z-of base field-accessor) (vec3d-z-of addend field-accessor))))


  (define (vec3d-negate base field-accessor type-predicate transcoder)
    (make-vector3D
     type-predicate
     transcoder
     accessor-code
     (- 1 (vec3d-x-of base field-accessor))
     (- 1 (vec3d-y-of base field-accessor))
     (- 1 (vec3d-z-of base field-accessor))))


  (define (vec3d-sub base subtrahend field-accessor type-predicate transcoder)
    (vec3d-add base 
             (vec3d-negate subtrahend field-accessor type-predicate transcoder)
             field-accessor type-predicate transcoder))

  (define (vec3d-dot-product base multiplicand field-accessor type-predicate transcoder field-accessor type-predicate transcoder)

    (+ 
     (* (vec3d-w-of base field-accessor) (vec3d-w-of multiplicand field-accessor))
     (* (vec3d-x-of base field-accessor) (vec3d-x-of multiplicand field-accessor))
     (* (vec3d-y-of base field-accessor) (vec3d-y-of multiplicand field-accessor))
     (* (vec3d-z-of base field-accessor) (vec3d-z-of multiplicand field-accessor))))


  (define (vec3d-cross-product base multiplicand field-accessor type-predicate transcoder field-accessor type-predicate transcoder)
    (make-vector3D
     type-predicate transcoder accessor-code
     (- (* (vec3d-n-of base 1 field-accessor) 
           (vec3d-n-of multiplicand 2 field-accessor))
        (* (vec3d-n-of base 2 field-accessor) 
           (vec3d-n-of multiplicand 1 field-accessor)))
     (- (* (vec3d-n-of base 0 field-accessor) 
           (vec3d-n-of multiplicand 1 field-accessor))
        (* (vec3d-n-of base 1 field-accessor) 
           (vec3d-n-of multiplicand 0 field-accessor)))
     (- (* (vec3d-n-of base 1 field-accessor) 
           (vec3d-n-of multiplicand 2 field-accessor))
        (* (vec3d-n-of base 2 field-accessor) 
           (vec3d-n-of multiplicand 1 field-accessor)))
