#!r6rs

(import 
  (rnrs base (6))
  (rnrs (6))
  (rnrs control (6))
  (rnrs io simple (6))

  ;; rgb-color type
  (colors rgb-colors)
  (image-file-types ppm)
  ;; basic string formatting
  (srfi :28)
  ;; time/date functions
  (srfi :19))


(define (color-palette nx ny color-range)
  "A more direct transliteration using the (DO) form"
  (let ((ppm (make-ppm-record (open-output-file
                               (date->string (current-date 0) "palette-~5.ppm"))
                              (exact (ceiling nx))
                              (exact (ceiling ny))
                              color-range
                              ""))
        (offset (+ color-range 0.99)))
    (do ((j (- (ppm-height ppm) 1)
            (- j 1)))
        ((< j 0))
      (do ((i 0
              (+ i 1)))
          ((>= i (ppm-width ppm)))
        (let ((hue
               (make-rgb-color
                (*  (/ i (ppm-width ppm)) offset)
                (*  (/ j (ppm-height ppm)) offset)
                (* 0.2 offset))))
          (set! ppm (ppm-append-pixel ppm hue))))
      (set! ppm (ppm-terminate-row ppm)))
    (write-ppm ppm)))

(color-palette 800 400 255)

