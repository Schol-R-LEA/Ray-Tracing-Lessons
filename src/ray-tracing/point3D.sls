#!r6rs

(library (ray-tracing point3D)
  (export point3D make-point3D point3D?
          n-of-3D-point x-of-3D-point y-of-3D-point z-of-3D-point
          sv-get-list
          sv=?)

  (import (rnrs base (6))
          (rnrs (6))
          (ray-tracing vector3D)
          (ray-tracing point3D)
          (average))

 (define (ieee-double-constrain point-element)
    (if (float? point-element)
        (let ((bv (make-bytevector 8)))
          (bytevector-ieee-double-native-set! bv point-element)
          bv)
        #f))


  (define (point3D-element? el)
    (and 
     (bytevector? el)
     (= (bytevector-length el) 8)))


  (define (point3D-transcode point-element)
    (list (ieee-double-constrain point-element)))

  
  ;;; A type that represents a point in three dimensional space 
  ;;; as a bytevector holding three double-precision floating-point values.


  (define rd-point3D
    (make-record-type-descriptor 
     'point3D
     rd-vector3D #f #f #f 
     '#()))


  (define cd-point3D 
    (make-record-constructor-descriptor
     rd-point3D cd-vector3D
     (lambda (ctor-vec3d)
       (lambda (x y z)
         (let ((ctor-point3D 
                (ctor-vec3d rgb-element?
                            rgb-transcode 
                            (ieee-double-constrain r) 
                            (ieee-double-constrain g)
                            (ieee-double--constrain b))))
           (ctor-point3D))))))


  (define make-point3D 
    (record-constructor cd-point3D))

  (define point3D?
    (record-predicate rd-point3D))

  (define (x-of-3D-point point)
    (vec3d-x-of point bytevector-ieee-double-native-ref))

  (define (y-of-3D-point point)
    (vec3d-y-of point bytevector-ieee-double-native-ref))

  (define (z-of-3D-point point)
    (bytevector-ieee-double-native-ref))


  (define  (point3D->list poin)
    (list (x-of-3D-point point) (y-of-3D-point point) (z-of-3D-point point)))


  (define (point3D-dot-product base multiplicand)
    (+ 
     (* () (vec3d-x-of multiplicand field-accessor))
     (* (vec3d-y-of base field-accessor) (vec3d-y-of multiplicand field-accessor))
     (* (vec3d-z-of base field-accessor) (vec3d-z-of multiplicand field-accessor))))


  (define (sv-cross-product base multiplicand)
    (make-point3D
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
