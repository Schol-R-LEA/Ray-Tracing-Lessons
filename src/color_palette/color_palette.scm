#!r6rs

(import 
  (rnrs base (6))
  (rnrs io simple (6))

  ;; basic string formatting
  (srfi :28))


(define (color-palette nx ny color-range)
  (display (format "P3~%~a ~a~%~a~%" nx ny (exact (ceiling color-range))))
  
  (let outer ((j (- ny 1)))
    (let inner ((i 0) (y (exact (floor (* (/ j ny) (+ color-range 0.99))))))
      (display 
       (format 
        "~a ~a ~a~%" 
        (exact (floor (* (/ i nx) (+  color-range 0.99)))) y 51))
      
      (if (< i (-  nx 1)) 
          (inner (+ 1 i) y)))
    (if (>= j 1)
        (outer (- j 1)))))

(color-palette 200 100 255)
