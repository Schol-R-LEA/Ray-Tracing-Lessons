#!r6rs

(library (ray-tracing ray3D)
  (export ray3D make-ray3D ray3D?
          n-of-3D-ray x-of-3D-ray y-of-3D-ray z-of-3D-ray
          ray3D-get-list
          ray3D=?)

  (import (rnrs base (6))
          (rnrs (6))
          (ray-tracing vector3D)
          (average))


  (define ray3D 
    (make-record-type-descriptor
     'ray3D
     'vector3D
     #f #f #f
     '#((immutable byte-size)
        (immutable field-accessor)))


  (define (ray3D-dot-product base multiplicand)
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
