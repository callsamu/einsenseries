;;Matrix Utilities

;Multiply row elements by some constant
(define (multiply-row row constant) 
  (map (lambda (x) (* x constant)) row))

;Divides row elements by some constant
(define (divide-row row constant) 
  (map (lambda (x) (/ x constant)) row))

;Combines two rows by summing each of their elements
(define (sum-rows r1 r2)
  (if (or (null? r1) (null? r2))
      nil
      (cons (+ (car r1) (car r2))
            (sum-rows (cdr r1) (cdr r2)))))

;Combines two rows by summing each of their elements
(define (product-rows r1 r2)
  (if (or (null? r1) (null? r2))
      nil
      (cons (* (car r1) (car r2))
            (product-rows (cdr r1) (cdr r2)))))

;Add each element of a list to a matrix row
(define (add-to-rows items matrix)
  (if (or (null? items) (null? matrix))
      nil
      (cons (cons (car items)
                  (car matrix))
            (add-to-rows (cdr items) (cdr matrix)))))

;Adds a new column and a new row
(define (cons-row-col col row matrix)
  (add-to-rows row (cons col matrix)))

;Returns first column
(define (car-col matrix)
  (if (null? matrix)
      nil
      (cons (car (car matrix))
            (car-col (cdr matrix)))))

;Drops first column
(define (cdr-col matrix)
  (map (lambda (row) 
         (if (null? row)
             row
             (cdr row)))
        matrix))

;Drop first row and column
(define (cdr-row-col matrix)
  (if (null? matrix)
      nil
      (cdr-col (cdr matrix))))

;Returns true if matrix has at least one null element
(define (null-in-matrix? matrix)
  (fold (map null?
             matrix)
        (lambda (p q) 
          (or p q))
        #f))

;These names are terrible, i know.
