#!r6rs

(library (average)
  (export avg)

  (import (rnrs base (6))
          (rnrs lists (6)))


  (define (avg n . x)
    (/ (fold-left + n x) (+ 1 (length x)))))
