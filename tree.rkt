#lang racket

; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define no-more? empty?)

(define except-first-denomination cdr)

(define first-denomination car)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

; Exercise 2.20
(define (same-parity first . rest)
  (let ([parity (even? first)])
    (define (same-parity-iter next partial)
      (cond [(empty? next) partial]
            [(xor parity (even? (car next))) (same-parity-iter (cdr next) partial)]
            [else (same-parity-iter (cdr next) (append partial (list (car next))))]))
    (same-parity-iter rest (list first))))

; Exercise 2.21
(define (square-list-1 items)
  (if (null? items)
      (list)
      (append (list (sqr (car items))) (square-list-1 (cdr items)))))

(define (square-list items)
  (map sqr items))

; Exercise 2.22
(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons answer (sqr (car things))))))
  (iter items null))

; Exercise 2.23
(define (for-each procedure data)
  (if (empty? data)
      (begin
        (newline)
        (display "Finished!")
        true)
      (begin
        (procedure (car data))
        (for-each procedure (cdr data)))))

; Exercise 2.25
(define example1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr example1)))))
(define example2 (cons (cons 7 null) null))
(car (car example2))
(define example3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr example3))))))))))))

; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

; Exercise 2.27
(define (deep-reverse x)
  (if (list? x)
      (reverse (map deep-reverse x))
      x))

; Exercise 2.28
(define (fringe subtree)
  (if (list? subtree)
      (append (fringe (car subtree)) (fringe (car (cdr subtree))))
      (list subtree)))

(define tr (list (list 1 2) (list 3 4)))
(fringe tr)
(fringe (list tr tr))

