#!r6rs 

(import 
 (rnrs base (6))
 ;; composite standard library, imports most std libs
 (rnrs (6))  
 ;; unit testing
 (srfi :64)
 ;; keywords
 (srfi :88)  
 (ray-tracing types))

(define runner (test-runner-simple))

(test-with-runner 
 runner
 (test-group 
  "Tests of support functions and data types"
  
