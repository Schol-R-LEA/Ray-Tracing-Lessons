#!r6rs 

(import 
  (rnrs base (6))
  ;; composite standard library, imports most std libs
  (rnrs io simple (6))
  ;; basic string formatting
  (srfi :28)
  (ray-tracing colors))


(define (make-ppm-solid-line color width)
  (let ((color-values (string-append 
                       (rgb-color->numeric-string color) 
                       " ")))
    (let loop ((w width))
      (cond ((< w 1) "")
            ((= w 1) color-values)
            (else (string-append color-values
                                 (loop (- w 1))))))))


(define (make-ppm-simple-solid-swatch color width height)
  (let ((line (string-append (make-ppm-solid-line color width) "\n")))
    (let loop ((h height))
      (cond ((< h 1) "")
            ((= h 1) line)
            (else (string-append line 
                                 (loop (- h 1))))))))


(define (make-ppm-solid-swatch-generator color width height)
  (let ((recur
         (lambda (c w h)
           (cons 
            (make-ppm-solid-line color width)
            ((lambda (y) 
               (recur c w y))
             (- h 1))))))
    recur))


#|
(display "P3\n1000 800\n255\n\n")
(display (make-ppm-simple-solid-swatch (make-rgb-color 127 0 255) 1000 800))
(newline)
|#

(define (generate-ppm-header width height hues pixels)
  (format "P3~%~a ~a~%~a~%~a"
          width height hues pixels))

(display (generate-ppm-header 1000 800 255 
                              (make-ppm-simple-solid-swatch 
                               (make-rgb-color 127 0 255) 1000 800)))
