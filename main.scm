(load "utils.scm")
(load "system-solver.scm")
(load "series-calculator.scm")

(define FILENAME "result.csv")
(define LIMIT 100)
(define SEP ",")

(call-with-output-file FILENAME
  (lambda (out)
    (for-each (lambda (lst) (print-list lst SEP out))
              (map (lambda (n) 
                   (cons n (get-solution n)))
                   (enumerate 0 LIMIT)))))
