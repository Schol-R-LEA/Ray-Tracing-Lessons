#!r6rs 

(import 
  (rnrs base (6))
  ;; composite standard library, imports most std libs
  (rnrs (6))  
  ;; unit testing
  (srfi :64)
  (average))


(define runner (test-runner-simple))

(test-with-runner 
    runner
  (test-group 
   "Test homebrew average procedure"
   (test-equal (/ 2 2) (avg 1 1))
   (test-equal (/ 1 2) (avg 1 0))
   (test-equal (/ 1 3) (avg 1 0 0))
   (test-equal (/ 4 3) (avg 4 0 0))
   (test-equal 0 (avg -1 1))
   (test-equal 10 (avg 5 15))
   (test-equal 5 (avg 5 5 5 5 5))
   (test-equal (/ (+ 5 10 15) 3) (avg 5 10 15))
   (test-equal (/ (+ 2 3 5 17 23 42 69 108) 8) (avg  2 3 5 17 23 42 69 108))
   (test-equal (/ (+ 3.1415 2.18 1.4142) 3) (avg 3.1415 2.18 1.4142))))
