(load "utils.scm")
(load "matrix-utils.scm")

;Writes a matrix in row echelon form
(define (row-echelon matrix)
  (define (clear-column matrix)
    (let ((first-row (divide-row (car matrix)
                                 (car (car matrix)))))
     (cons first-row 
           (map (lambda (row) 
                  (sum-rows row 
                            (multiply-row first-row 
                                          (* -1 (car row)))))
                (cdr matrix)))))

  (if (or (null? matrix)
          (null-in-matrix? matrix))
      matrix
      (let ((pivots (filter (lambda (r) 
                              (not (= 0 (car r))))
                            matrix))
            (non-pivots (filter (lambda (r) 
                                  (= 0 (car r)))
                                matrix)))
        (if (null? pivots)
            (add-to-rows (car-col matrix)
                         (row-echelon (cdr-col matrix)))
            (let ((clear-matrix (append (clear-column pivots)
                                        non-pivots)))
              (cons-row-col (cdr (car clear-matrix))
                            (car-col clear-matrix)
                            (row-echelon (cdr-row-col clear-matrix))))))))

;;Performs backward substitution on a row-echelon form matrix
(define (back-sub matrix)
  (if (null? matrix)
      nil
      (let ((solutions (back-sub (cdr-row-col matrix))))
        (cons (- (last (car matrix))
                 (sum (product-rows (pop (cdr (car matrix)))
                                    solutions)))
              solutions))))

;Solves a system of equations represented as an augmented matrix
(define (solve-system aug-matrix)
  (back-sub (row-echelon aug-matrix)))

