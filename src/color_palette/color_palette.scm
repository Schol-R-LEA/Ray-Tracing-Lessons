#!r6rs

(import 
  (rnrs base (6))
  (rnrs io simple (6)))


(define (color-palette nx ny ratio)
  (display "P3")
  (newline)
  (display nx)
  (display #\space)
  (display ny)
  (newline)
  (display (exact (ceiling ratio)))
  (newline)
  
  (let outer ((j (- ny 1)))
    (let inner ((i 0) (y (exact (floor (* (/ j ny) (+  ratio 0.99))))))
      (display (exact (floor (* (/ i nx) (+  ratio 0.99)))))
      (display #\space)
      (display y)
      (display #\space)
      (display 51)
      (newline)
      (if (< i (-  nx 1)) 
          (inner (+ 1 i) y)
          '()))
    (if (>= j 1)
        (outer (- j 1)))))

(color-palette 200 100 255)
