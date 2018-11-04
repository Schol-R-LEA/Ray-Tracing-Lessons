#!r6rs

(library (ray-tracing spacial-vectors)
  (export spacial-vector3D make-spacial-vector3D spacial-vector3D?
          sv-n-of sv-x-of sv-y-of sv-z-of
          sv-get-list
          sv=? sv-scale sv-scalar-sum
          sv-negate sv-add sv-sub
          sv-average
          sv-dot-product sv-cross-product)

  (import (rnrs base (6))
          (rnrs (6))
          (ray-tracing vector3D)
          (average))


  (define spacial-vector3D 
    (make-record-type-descriptor
     'spacial-vector3D
     'vector3D
     #f #f #f
     '#((immutable byte-size)
        (immutable field-accessor)))


  (define (sv-dot-product base multiplicand)
    (+ 
     (* (vec3d-x-of) (vec3d-x-of multiplicand field-accessor))
     (* (vec3d-y-of base field-accessor) (vec3d-y-of multiplicand field-accessor))
     (* (vec3d-z-of base field-accessor) (vec3d-z-of multiplicand field-accessor))))


  (define (sv-cross-product base multiplicand field-accessor type-predicate transcoder field-accessor type-predicate transcoder)
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
           (vec3d-n-of multiplicand 1 field-accessor)))))
