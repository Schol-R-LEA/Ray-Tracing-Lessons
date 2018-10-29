#!r6rs 

(import 
  (rnrs base (6))
  ;; composite standard library, imports most std libs
  (rnrs (6))
  (ray-tracing types))


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
     (display sub-list)
     (newline))
   lst))

(tabulate-list 
 (permute-list 
  (map (lambda (color)
         (get-rgb-list color))
       (list rgb-red rgb-green rgb-blue rgb-white rgb-black))))
