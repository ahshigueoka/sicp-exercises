#lang racket


(define (flatmap proc seq)
  (foldr append null (map proc seq)))


(define (adjoin-position new-row rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? col positions)
  (if (null? positions)
      true
      (let ([row (car positions)])
        (define (check-two-queens row-placed col-placed)
          (or
           (= row-placed row)
           (= (abs (- row-placed row)) (abs (- col-placed col)))))
        (not (ormap check-two-queens
                    (cdr positions)
                    (inclusive-range (- col 1) 1 -1))))))

(define empty-board null)


; In the recommended approach, the program calls
; one adjoin for each board size and for each element
; of the solution in the subproblem
; S(k) is the number of solutions, C(k) is the time complexity
;
; For k in 1..N
;     C(k) = C(k-1) + N*S(k-1)
;
; In Louis' approach, the program calls adjoin for
; each solution of C(N-1), for each board size
; and there is also the cost of calling the subproblem
; for each N
;
; For k in 1..N
;     C(k) = N*C(k-1) + N*S(k-1)
; 
(define (queen-cols k board-size)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row rest-of-queens))
                 (inclusive-range 1 board-size)))
          (queen-cols (- k 1) board-size)))))


(define (queens board-size)
  (queen-cols board-size board-size))

(provide adjoin-position
         safe?
         empty-board
         queen-cols
         queens)