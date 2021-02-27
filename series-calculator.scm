(load "utils.scm")
(load "system-solver.scm")

(define get-solution
 (let ((solutions '((0 (1 1 1))
                    (1 (-24 240 -504)))))
   (lambda (n)
     (let ((S (assq n solutions)))
       (if (eq? S #f)
           (begin (set! solutions (cons (list n (solve-abc n))
                                  solutions))
                  (get-solution n))   
                  (cadr S))))))

(define (solve-abc n)
  (define (a n) (first  (get-solution n)))
  (define (b n) (second (get-solution n)))
  (define (c n) (third  (get-solution n)))
  
  (define (sum-term f g)
    (sigma 1 
           (- n 1)
           (lambda (k) 
             (* (f k)
                (g (- n k))))))
  
  (let ((c1 (sum-term a a))
        (c2 (sum-term a b))
        (c3 (- (sum-term c a)
               (sum-term b b))))
    (solve-system (list (list (- (* 12 n) 2)       1            0          c1)
                        (list       -1       (- (* 3 n) 1)      1          c2)
                        (list       -1             2       (- (* 2 n) 1)   c3)))))
