#!r6rs 

(import 
  (rnrs base (6))
  ;; composite standard library, imports most std libs
  (rnrs (6))
  ;; simple I/O library
  (rnrs io simple (6))
  ;; unit testing
  (srfi :64)
  ;; basic string formatting
  (srfi :28)
  ;; time/date functions
  (srfi :19)  
  (colors rgb-colors)
  (image-file-types ppm))

(define runner (test-runner-simple))

(test-with-runner 
    runner
  (test-group
   "Tests of ppm-record type"
   (test-group
    "minimal color swatch"
    (let* ((test-file-name (format "swatch-~A.ppm"
                                   (date->string (current-date 0) "~5")))
           (test-file (open-output-file test-file-name))
           (test-ppm (make-ppm-record test-file 500 500 255 "")))
      (ppm-record? test-ppm)
      (test-assert (string=? (ppm-data test-ppm)
                             "P3\n500 500\n255\n"))
      (test-assert (string=? (ppm-data (ppm-terminate-row test-ppm))
                             "P3\n500 500\n255\n\n"))
      (test-assert (string=? (ppm-data (ppm-append-pixel test-ppm rgb-blue))
                             "P3\n500 500\n255\n0 0 255  "))
      (let ((test-ppm-final
             (let outer ((j 0)
                         (outer-ppm test-ppm))
               (if (>= j 500)
                   outer-ppm
                   (outer (+ 1 j)
                           (let inner ((i 0)
                                       (inner-ppm outer-ppm))
                             (if (>= i 500)
                                 (ppm-terminate-row inner-ppm)
                                 (inner (+ 1 i)
                                        (ppm-append-pixel inner-ppm rgb-blue)))))))))
        (put-string (ppm-file-handle test-ppm-final) (ppm-data test-ppm-final))
        (close-output-port (ppm-file-handle test-ppm-final)))))))
