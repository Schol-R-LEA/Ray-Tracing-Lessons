#!r6rs

(library (ray-tracing vector3D)
  (export rd-vector3D cd-vector3D
          make-vector3D 
          ;; make-vector3D-from-bytevector
          vector3D?
          vector3D-get-bytevector vector3D-n-of
          vec3d-x-of vec3d-y-of vec3d-z-of         
          vector3D-list
          vec3d=? vec3d-scale vec3d-scalar-sum
          vec3d-negate vec3d-add vec3d-sub 

          bytevector-accessors

          &invalid-type-predicate-violation
          make-invalid-type-predicate-violation
          invalid-type-predicate-violation?
          &invalid-transcoder-violation
          make-invalid-transcoder-violation
          invalid-transcoder-violation?
          &invalid-accessor-code-violation
          make-invalid-accessor-code-violation
          invalid-accessor-code-violation?          
          &vector3D-type-constraint-violation
          make-vector3D-type-constraint-violation 
          vector3D-type-constraint-violation?
          )

  (import (rnrs base (6))
          ;; composite standard lbrary, imports most std libs
          (rnrs (6)))


  (define-condition-type &invalid-type-predicate-violation
    &syntax 
    make-invalid-type-predicate-violation
    invalid-type-predicate-violation?)


  (define-condition-type &invalid-transcoder-violation
    &syntax 
    make-invalid-transcoder-violation
    invalid-transcoder-violation?)

  (define-condition-type &invalid-accessor-code-violation
    &syntax 
    make-invalid-accessor-code-violation
    invalid-accessor-code-violation?)

  (define-condition-type &vector3D-type-constraint-violation
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

  (define rd-vector3D 
    (make-record-type-descriptor 
     'vector3D #f #f #f #f
     '#((immutable coordinate-elements))))


  (define cd-vector3D
    (make-record-constructor-descriptor 
     rd-vector3D #f
     (lambda (ctor)
       (lambda (type-predicate u8-transcoder x y z)
         (cond ((not (procedure? type-predicate))
                (raise (make-invalid-type-predicate-violation
                        (make-message-condition 
                         "vector3D: given type predicate not a valid procedure."))))
               ((not (procedure? u8-transcoder))
                (raise (make-invalid-transcoder-violation
                        "vector3D: not a valid bytevector transcoder.")))
               ((not (and
                      (type-predicate x) 
                      (type-predicate y)
                      (type-predicate z)))
                (raise (make-vector3D-type-constraint-violation
                        "vector3D: invalid ctor arguments.")))
               (else
                (let ((xyz-list 
                       (append 
                        (u8-transcoder x)
                        (u8-transcoder y)
                        (u8-transcoder z))))
                  (ctor 
                   (u8-list->bytevector xyz-list)))))))))
  
  (define make-vector3D
    (record-constructor cd-vector3D))
  
  
  #|
  (define make-vector3D-from-bytevector
  (record-constructor
  (make-record-constructor-descriptor 
  'vector3D #f
  (lambda (ctor-vec3D/bv)
  (lambda (type-predicate accessor-code bv)
  (let ((accessor (assoc accessor-code accessor-lookup-table)))
  (cond ((not (procedure? type-predicate))
  (raise (make-invalid-type-predicate-violation
  (make-message-condition 
  "vector3D: given type predicate not a valid procedure."))))
  ((not accessor)
  (raise (make-invalid-accessor-code-violation
  (make-message-condition 
  "vector3D: accessor type not valid."))))
  ((not (and
  (type-predicate (accessor bv 0)) 
  (type-predicate (accessor bv 1))
  (type-predicate (accessor bv 2))))
  (raise (make-vector3D-type-constraint-violation
  "vector3D: invalid ctor arguments.")))
  
  (else
  (ctor-vec3D/bv (bytevector-copy bv))))))))))
  |#
  
  (define vector3D?
    (record-predicate rd-vector3D))
  

  (define vector3D-get-bytevector
    (record-accessor rd-vector3D 0))

  #|

  (define (vec3D-copy vec type-predicate accessor-code)
  (make-vector3D-from-bytevector type-predicate 
  accessor-code 
  (bytevector-copy vector3D-get-bytevector)))

  |#

  (define (vector3D-n-of q n field-accessor)
    (field-accessor (vector3D-get-bytevector) n))

  (define (vec3d-x-of q field-accessor) 
    (vector3D-n-of q 0 field-accessor))

  (define (vec3d-y-of q field-accessor) 
    (vector3D-n-of q 1 field-accessor))

  (define (vec3d-z-of q field-accessor) 
    (vector3D-n-of q 2 field-accessor))


  (define  (vector3D-list q field-accessor)
    (list (vec3d-x-of q field-accessor)
          (vec3d-y-of q field-accessor)
          (vec3d-z-of q field-accessor)))

  (define (vec3d=? base compared field-accessor)
    (and
     (= (vec3d-x-of base field-accessor) (vec3d-x-of compared field-accessor))
     (= (vec3d-y-of base field-accessor) (vec3d-y-of compared field-accessor))
     (= (vec3d-z-of base field-accessor) (vec3d-z-of compared field-accessor))))


  (define (vec3d-scale base scaling-factor field-accessor type-predicate transcoder)
    (make-vector3D
     type-predicate
     transcoder
     (* (vec3d-x-of base field-accessor) scaling-factor)
     (* (vec3d-y-of base field-accessor) scaling-factor)
     (* (vec3d-z-of base field-accessor) scaling-factor)))

  (define (vec3d-scalar-sum base addend field-accessor type-predicate transcoder)
    (make-vector3D
     type-predicate
     transcoder   
     (* (vec3d-x-of base field-accessor) addend)
     (* (vec3d-y-of base field-accessor) addend)
     (* (vec3d-z-of base field-accessor) addend)))


  (define (vec3d-add base addend field-accessor type-predicate transcoder)
    (make-vector3D
     type-predicate
     transcoder 
     (+ (vec3d-x-of base field-accessor) (vec3d-x-of addend field-accessor))
     (+ (vec3d-y-of base field-accessor) (vec3d-y-of addend field-accessor))
     (+ (vec3d-z-of base field-accessor) (vec3d-z-of addend field-accessor))))


  (define (vec3d-negate base field-accessor type-predicate transcoder)
    (make-vector3D
     type-predicate
     transcoder
     (- 1 (vec3d-x-of base field-accessor))
     (- 1 (vec3d-y-of base field-accessor))
     (- 1 (vec3d-z-of base field-accessor))))


  (define (vec3d-sub base subtrahend field-accessor type-predicate transcoder)
    (vec3d-add base 
               (vec3d-negate subtrahend field-accessor type-predicate transcoder)
               field-accessor type-predicate transcoder)))
