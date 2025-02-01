#lang racket
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (fr n)
  (if (< n 3)
      n
      (+ (fr (- n 1))
         (* 2 (fr (- n 2)))
         (* 3 (fr (- n 3))))))

(define (fn a b c)
  (+ a (* 2 b) (* 3 c)))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (fi n)
  (f-iter 2 1 0 3 n))

(define (f-iter prev prev2 prev3 curr n)
  (cond [(< n 3) n]
        [(= curr n) (fn prev prev2 prev3)]
        [else (f-iter (fn prev prev2 prev3) prev prev2 (inc curr) n)]))

(define (pascal row col)
  (cond [(< col 0) 0]
        [(< row 0) 0]
        [(< row col) 0]
        [(= row col) 1]
        [(= col 0) 1]
        [else (+
               (pascal (dec row) (dec col))
               (pascal (dec row) col))]))