#!r6rs 

(import 
  (rnrs base (6))
  ;; composite standard library, imports most std libs
  (rnrs (6))  
  ;; unit testing
  (srfi :64)
  ;; basic string formatting
  (srfi :28)
  (ray-tracing point3D))


(define (display-point3D vec)
  (display (format "<~A, ~A, ~A>~%" (x-of vec) (y-of vec) (z-of vec))))

(define runner (test-runner-simple))

(test-with-runner 
    runner
  (test-group
   "Tests of point3D data type and procedures"
   (test-group
    "Test creation of point3D variable"
    (let ((vec-1 (make-point3D 1.0 -2.1 3.14))
          (vec-2 (make-point3D 6.0 4.45 -7.0)))
      (test-assert (point3D? vec-1))
      (test-equal 1.0 (x-of vec-1))
      (test-equal -2.1 (y-of vec-1))
      (test-equal 3.14 (z-of vec-1))
      (test-group
       "Test comparing point3D variable"  
       (let ((vec-comp (make-point3D 1.0 -2.1 3.14)))
         (test-assert (point3D=? vec-1 vec-comp))))
      (test-group
       "Test copying point3D variable"
       (let ((vec-copy (point3D-copy vec-1)))
         (test-assert (point3D=? vec-1 vec-copy))))
      (test-group
       "test point3D mathematical operations"
       (test-group
        "addition and subtraction"
        (let ((vec-add (make-point3D (+ 1.0 6.0) (+ -2.1 4.45) (+ 3.14 -7.0)))
              (vec-sub (make-point3D  (- 1.0 6.0) (- -2.1 4.45) (- 3.14 -7.0))))
          (test-assert (point3D=? (point3D-add vec-1 vec-2) vec-add))           
          (test-assert (point3D=? (point3D-subtract vec-1 vec-2) vec-sub))))

       (test-group
        "multiplication and division"
        (let ((vec-*5 (make-point3D 5.0 5.0 5.0))
              (vec-mult (make-point3D  (* 1.0 5.0) (* -2.1 5.0) (* 3.14 5.0)))
              (vec-mult2 (make-point3D (* 1.0 6.0) (* -2.1 4.45) (* 3.14 -7.0)))
              (vec-div (make-point3D (/ 1.0 5.0) (/ -2.1 5.0) (/ 3.14 5.0)))
              (vec-div2 (make-point3D  (/ 1.0 6.0) (/ -2.1 4.45) (/ 3.14 -7.0))))
          (test-assert (point3D=? (point3D-scalar-multiply vec-1 5.0) vec-mult))
          (test-assert (point3D=? (point3D-flat-multiply vec-1 vec-*5) vec-mult))
          (test-assert (point3D=? (point3D-flat-multiply vec-1 vec-2) vec-mult2))
          (test-assert (point3D=? (point3D-scalar-divide vec-1 5.0) vec-div))
          (test-assert (point3D=? (point3D-flat-divide vec-1 vec-*5) vec-div))
          (test-assert (point3D=? (point3D-flat-divide vec-1 vec-2) vec-div2))))
       
       (test-group
        "dot product"
        (let ((dot-product-1-2 (+ (* 1.0 6.0) (* -2.1 4.45) (* 3.14 -7.0))))
          (test-equal (point3D-dot-product vec-1 vec-2) dot-product-1-2)))
       
       (test-group
        "cross product"
        (let ((vec-cross-1-2 (make-point3D
                              (- (* -2.1 -7.0)
                                 (* 3.14 4.45))
                              (- (- (* 1.0 -7.0)
                                    (* 3.14 6.0)))
                              (- (* 1.0 4.45)
                                 (* 6.0 -2.1))))) 
          (test-assert (point3D=? (point3D-cross-product vec-1 vec-2) vec-cross-1-2)))))))))
