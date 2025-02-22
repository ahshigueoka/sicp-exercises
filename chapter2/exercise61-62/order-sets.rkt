#lang racket/base

(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(= x (car set)) #t]
        [(< x (car set)) #f]
        [else (element-of-set? x (cdr set))]))

; Exercise 2.61
(define (adjoin-set x set)
  (define (insert-set previous next)
    (cond [(null? next) (append previous (list x))]
          [(< x (car next)) (append previous (cons x next))]
          [(> x (car next)) (insert-set (append previous (list (car next))) (cdr next))]
          [else (append previous next)]))
  (insert-set '() set))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([min1 (car set1)]
            [min2 (car set2)])
        (cond [(< min1 min2) (intersection-set (cdr set1) set2)]
              [(> min1 min2) (intersection-set set1 (cdr set2))]
              [else (cons min1 (intersection-set (cdr set1) (cdr set2)))]))))

; Exercise 2.62
(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else (let ([min1 (car set1)]
                    [min2 (car set2)])
                (cond [(< min1 min2) (cons min1 (union-set (cdr set1) set2))]
                      [(> min1 min2) (cons min2 (union-set set1 (cdr set2)))]
                      [else (cons min1 (union-set (cdr set1) (cdr set2)))]))]))

(provide element-of-set?
         adjoin-set
         intersection-set
         union-set)