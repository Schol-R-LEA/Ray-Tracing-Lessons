#!r6rs

(library (image-file-types ppm)
  (export ppm-record make-ppm-record ppm-record?
          ppm-file-handle ppm-width ppm-height ppm-colors ppm-data
          ppm-append-pixel ppm-terminate-row write-ppm)
  (import (rnrs base (6))
          ;; composite standard lbrary, imports most std libs
          (rnrs (6))
          (rnrs io simple (6))
          ;; rgb-color type
          (colors rgb-colors)
          ;; basic string formatting
          (srfi :28))


  (define-record-type (ppm-record make-ppm-record ppm-record?)
    (fields 
     (immutable file-handle ppm-file-handle)
     (immutable viewport-width ppm-width)
     (immutable viewport-height ppm-height)
     (immutable color-range ppm-colors)
     (immutable ppm-data-string ppm-data))
    (protocol
     (lambda (ctor)
       (lambda (dest x y clr ppm)
         (ctor dest
               x
               y
               clr
               (if (and (string? ppm)
                        (not (string=? ppm "")))
                   ppm
                   (format "P3~%~a ~a~%~a~%"
                           (exact (ceiling x))
                           (exact (ceiling y))
                           (exact (ceiling clr)))))))))


  (define (ppm-append-pixel ppmr color)
    (make-ppm-record (ppm-file-handle ppmr)
                     (ppm-width ppmr)
                     (ppm-height ppmr)
                     (ppm-colors ppmr)
                     (string-append (ppm-data ppmr)
                                    (string-append (rgb-color->numeric-string color)
                                                   " "))))


  (define (ppm-terminate-row ppmr)
    (make-ppm-record (ppm-file-handle ppmr)
                     (ppm-width ppmr)
                     (ppm-height ppmr)
                     (ppm-colors ppmr)
                     (string-append (ppm-data ppmr) "\n")))


  (define (write-ppm ppm)
    (put-string (ppm-file-handle ppm) (ppm-data ppm))
    (close-output-port (ppm-file-handle ppm))))

