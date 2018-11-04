#!r6rs

(import 
  (rnrs base (6))
  (rnrs control (6))
  (rnrs io simple (6))

  ;; rgb-color type
  (ray-tracing colors)

  ;; basic string formatting
  (srfi :28))


(define (color-palette nx ny color-range)
  "A more direct transliteration using the (DO) form"
  (display (format "P3~%~a ~a~%~a~%" nx ny (exact (ceiling color-range))))
  (let  ((offset (+ color-range 0.99)))
    (do ((j (- ny 1) (- j 1)))
        ((< j 0))
      (do ((i 0  (+ i 1)))
          ((>= i nx))
        (let ((hue 
               (make-rgb-color 
                (* (/ i nx) offset)
                (* (/ j ny) offset)
                (* 0.2 offset))))
          (display (rgb-color->numeric-string hue))
          (newline))))))
  
(color-palette 200 100 255)
