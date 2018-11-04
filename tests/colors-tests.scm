#!r6rs 

(import 
  (rnrs base (6))
  ;; composite standard library, imports most std libs
  (rnrs (6))  
  ;; unit testing
  (srfi :64)
  (ray-tracing colors)
  (average))


(define (permute-list wd)
  "as per the solution given by 'Pie o Pah' in this S-O answer:
https://stackoverflow.com/questions/2710713/algorithm-to-generate-all-possible-permutations-of-a-list"
  (cond ((null? wd) '())
        ((null? (cdr wd)) (list wd))
        (else
         (let splice ((l '()) (m (car wd)) (r (cdr wd)))
           (append
            (map (lambda (x) (cons m x)) (permute-list (append l r)))
            (if (null? r)
                '()
                (splice (cons m l) (car r) (cdr r))))))))

(define (tabulate-list lst)
  (for-each
   (lambda (sub-list)
     (for-each
      (lambda (el)
        (display (rgb-color->list el)))
      sub-list)
     (newline))
   lst))


(define runner (test-runner-simple))

(test-with-runner 
    runner

  (test-group
   "Tests of rgb-color data type and procedures"
   (test-group 
    "Test creation of new color"
    (let ((maroon (make-rgb-color #xff 0 #xff)))
      ;; (display (red-of maroon))
      ;; (newline)
      (test-equal #xff (red-of maroon))
      (test-equal 0 (green-of maroon))
      (test-equal #xff (blue-of maroon))))
   
   (test-group 
    "Test values defined colors"
    (test-equal 0 (red-of rgb-black))
    (test-equal 0 (green-of rgb-black))
    (test-equal 0 (blue-of rgb-black))
    
    (test-equal #xff (red-of rgb-white))
    (test-equal #xff (green-of rgb-white))
    (test-equal #xff (blue-of rgb-white))
    
    (test-equal #xff (red-of rgb-red))
    (test-equal 0 (green-of rgb-red))
    (test-equal 0 (blue-of rgb-red))
    
    (test-equal 0 (red-of rgb-green))
    (test-equal #xff (green-of rgb-green))
    (test-equal 0 (blue-of rgb-green)) 
    
    (test-equal 0 (red-of rgb-blue))
    (test-equal 0 (green-of rgb-blue))
    (test-equal #xff (blue-of rgb-blue)))

   (test-group 
    "Test comparing colors"
    ;; a few manual tests to start
    (test-assert (color=? rgb-red rgb-red))
    (test-assert (not (color=? rgb-red rgb-green))))

   ;; generative test series
   (let* ((test-colors 
           (list rgb-white rgb-black 
                 rgb-red rgb-green rgb-blue))
          (stride (length test-colors))
          (endpoint (- stride 1)))
     (do ((matrix (permute-list test-colors))
          (comparison-list test-colors)
          (i 0 (+ i 1))) 
         ((> i endpoint))
       (do ((j 0 (+ j 1)))
           ((> j endpoint))
         (let* ((test-color (list-ref (list-ref matrix i) j))
                (comparison-color (list-ref comparison-list j))
                (test-result (color=? test-color comparison-color)))
           (display (rgb-color->list test-color))
           (test-assert (rgb-color? test-color))
           (test-assert 
               (if (or (= 0 i)
                       (< j (- endpoint i))) 
                   (begin
                     (display " == ")
                     (display (rgb-color->list comparison-color))
                     (newline)                     
                     test-result)
                   (begin 
                     (display " =/= ")
                     (display (rgb-color->list comparison-color))
                     (newline)
                     (not test-result))))))))

   (test-group 
    "Test values blending, additive mixing, 
     and subtractive mixing of colors"
    (let ((magenta (blend-colors rgb-red rgb-blue)))
      ;; (display "magenta: ")
      ;; (display (rgb-color->numeric-string magenta))
      ;; (newline)
      (test-equal #x7f (red-of magenta))
      (test-equal 0 (green-of magenta))
      (test-equal #x7f (blue-of magenta))
      (test-assert (color=? magenta rgb-magenta)))
    
    (let ((yellow (blend-colors rgb-red rgb-green)))
      ;; (display "yellow: ")
      ;; (display (rgb-color->numeric-string yellow))
      ;; (newline)
      (test-equal #x7f (red-of yellow))
      (test-equal #x7f (green-of yellow))
      (test-equal #x00 (blue-of yellow))
      (test-assert (color=? yellow rgb-yellow))

      (let ((maroon (add-colors rgb-red rgb-blue)))
        ;; (display "maroon: ")
        ;; (display (rgb-color->numeric-string maroon))
        ;; (newline)
        (test-equal #xff (red-of maroon))
        (test-equal 0 (green-of maroon))
        (test-equal #xff (blue-of maroon))
        (test-assert (color=? maroon rgb-maroon)))
      
      (let ((maroon (make-rgb-color #xff 0 #xff))
            (red-blue (add-colors rgb-red rgb-blue)))
        ;; (display "maroon: ")
        ;; (display (rgb-color->numeric-string maroon))
        ;; (newline)
        ;; (display "red-blue: ")
        ;; (display (rgb-color->numeric-string red-blue))
        ;; (newline)
        (test-equal (red-of maroon) (red-of red-blue))
        (test-equal (green-of maroon) (green-of red-blue))
        (test-equal (blue-of maroon) (blue-of red-blue))
        (test-assert (color=? maroon red-blue)))))
   
   (test-group 
    "Test returing PPM formatted colors"
    (test-assert (string=? "255 0 0" 
                           (rgb-color->numeric-string rgb-red))))))
