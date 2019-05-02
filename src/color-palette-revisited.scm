#!r6rs

(import (rnrs base (6))
        ;; composite standard lbrary, imports most std libs
        (rnrs (6))
        (ray-tracing types))

(define (format-ppm-header width height range)
  (format "P3~%~a ~a~%~a~%" width height (exact (ceiling range))))

(define (format-ppm-element)
  (format ""))
