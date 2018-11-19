#!r6rs 

(import 
  (rnrs base (6))
  ;; composite standard library, imports most std libs
  (rnrs (6))  
  ;; unit testing
  (srfi :64)
  (ray-tracing vector3D))


(define runner (test-runner-simple))

(test-with-runner 
    runner
  (test-group
   "Tests of vector3D data type and procedures"
   (test-group 
    "Test creation of new vector"
    (let* ((vec-u8 (make-vector3D integer? 
                                  (lambda (n) (list  n)) 
                                  #xff 0 #x7f))
           (w (vector3D-n-of vec-u8 2 bytevector-u8-ref))
           (x (vec3d-x-of vec-u8 bytevector-u8-ref))
           (y (vec3d-y-of vec-u8 bytevector-u8-ref))
           (z (vec3d-z-of vec-u8 bytevector-u8-ref)))
      (test-assert (vector3D? vec-u8))
      (display w)
      (newline)
      (display x)
      (newline)
      (display y)
      (newline)
      (display z)
      (newline)
      (test-equal #x7f w)
      (test-equal #xff x)
      (test-equal 0 y)
      (test-equal #x7f z)))

   (test-group 
    "Test creation of new vector from bytevector"
    (let* ((vec-u8 (make-vector3D-from-bytevector
                    (u8-list->bytevector '(#xff 0 #x7f))))
           (w (vector3D-n-of vec-u8 2 bytevector-u8-ref))
           (x (vec3d-x-of vec-u8 bytevector-u8-ref))
           (y (vec3d-y-of vec-u8 bytevector-u8-ref))
           (z (vec3d-z-of vec-u8 bytevector-u8-ref)))
      (test-assert (vector3D? vec-u8))
      (display w)
      (newline)
      (display x)
      (newline)
      (display y)
      (newline)
      (display z)
      (newline)
      (test-equal #x7f w)
      (test-equal #xff x)
      (test-equal 0 y)
      (test-equal #x7f z)))))
