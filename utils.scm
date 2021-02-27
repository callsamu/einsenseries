;;General utilities

;Shorthand For Empty List
(define nil '())

;Shorthands For Getting One of The First Three Elements
(define first car)
(define second cadr)
(define third (lambda (l) (cadr (cdr l))))

;Outputs a list in CSV format
(define (print-list lst sep out) ;!sep is the separator string
  (if (null? lst)
      (newline out)
      (begin (display (car lst) out)
             (if (not (null? (cdr lst)))
                 (display sep out))
             (print-list (cdr lst) sep out))))

;Returns Last Element from a List
(define (last list)
  (if (null? (cdr list))
      (car list)
      (last (cdr list))))

;Removes Last Element from a List
(define (pop list)
  (cond ((null? list) nil)
        ((null? (cdr list)) nil)
        (else (cons (car list) 
                    (pop (cdr list))))))

;Fold Higher-Order Procedure
(define (fold list comb neutral)
  (if (null? list)
      neutral
      (comb (car list)
            (fold (cdr list)
                  comb
                  neutral))))

;List Filter Procedure      
(define (filter p list)
  (cond ((null? list) nil)
        ((p (car list)) 
          (cons (car list) 
                (filter p (cdr list))))
        (else (filter p (cdr list)))))

;Enumerates numbers from a to b
(define (enumerate a b)
  (if (> a b)
      nil
      (cons a (enumerate (+ a 1) b))))

;Sums numbers of a list
(define (sum list)
  (fold list
        +
        0))

;Representation of the Sigma Notation 
(define (sigma a b f)
  (sum (map f (enumerate a b))))
