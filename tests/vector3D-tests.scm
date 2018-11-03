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
    "Test creation of new color"
    (let* ((vec-u8 (make-vector3D integer? 
                                  (lambda (n) n) 
                                  '(#xff) '(0) '(#xff)))
           (x (vec3d-)))
     
      (newline)
      (test-equal #xff x)
      (test-equal 0 y)
      (test-equal #xff z)))))
