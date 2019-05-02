#!r6rs 

(import 
  (rnrs base (6))
  ;; composite standard library, imports most std libs
  (rnrs (6))  
  ;; unit testing
  (srfi :64)
  ;; basic string formatting
  (srfi :28)
  (ray-tracing point3D)
  (ray-tracing ray3D))


(define (format-point3D vec)
  (format "<~A, ~A, ~A>" (x-of vec) (y-of vec) (z-of vec)))


(define (format-ray3D ray)
  (format "{~A -> ~A}~%"
          (format-point3D (origin-get ray))
          (format-point3D (direction-get ray))))


(define runner (test-runner-simple))

(test-with-runner 
    runner
  (test-group
   "Tests of ray3D data type and procedures"
   (test-group
    "Test creation of ray3D variables"
    (let ((ray-1 (make-ray3D (make-point3D 1.0 2.0 -3.0)
                             (make-point3D 5.0 6.0 -7.0))))
      (test-assert (ray3D? ray-1))
      (test-group
       "Test comparisons of ray3D variables"
       (let ((ray-comp (make-ray3D (make-point3D 8.0 9.0 -10.0)
                                   (make-point3D 11.0 12.0 -13.0))))
         (test-assert (ray3D=? ray-1
                               (make-ray3D (make-point3D 1.0 2.0 -3.0)
                                           (make-point3D 5.0 6.0 -7.0))))
         (test-assert (not (ray3D=? ray-1 ray-comp)))))
      (test-group
       "Test copying of ray3D variables"
       (let ((ray-dup (ray3D-copy ray-1)))
         (test-assert (ray3D=? ray-1 ray-dup))))
      (test-group
       "Computing a given point on a given ray3D"
       (let ((test-point (ray3D-point-at ray-1 2))
             (comp-point (point3D-add (origin-get ray-1)
                                      (point3D-scalar-multiply (direction-get ray-1) 2))))
         (test-assert (point3D? test-point))
         (test-assert (point3D=? test-point comp-point))))
            (test-group
       "Computing a given point on a ray3D starting at the camera focal"
       (let* ((ray-2 (make-ray3D camera-focal-point
                                 (make-point3D 5.0 6.0 -7.0)))
              (test-point (ray3D-point-from-camera-at ray-1 1.5))
              (comp-point (ray3D-point-at ray-2 1.5)))
         (test-assert (point3D? test-point))
         (test-assert (point3D=? test-point comp-point))))))))
