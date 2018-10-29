#!r6rs

(import 
  (rnrs base (6))
  (rnrs control (6))
  (rnrs io simple (6))

  ;; basic string formatting
  (srfi :28))


(define (color-palette-do nx ny color-range)
  "A more direct transliteration using the (DO) form"
  (display (format "P3~%~a ~a~%~a~%" nx ny (exact (ceiling color-range))))
  (do ((j (- ny 1) (- j 1)))
      ((< j 0))
    (let ((y (exact (floor (* (/ j ny) (+ color-range 0.99))))))
      (do ((i 0  (+ i 1)))
          ((>= i nx))
        (display 
         (format 
          "~a ~a ~a~%" 
          (exact (floor (* (/ i nx) (+  color-range 0.99)))) y 51))))))


(color-palette-do 200 100 255)
