#lang racket

; Exercise 2.17
(define (last-pair list-in)
  (cond [(null? list-in) null]
        [(null? (cdr list-in)) (car list-in)]
        [else (last-pair (cdr list-in))]))

; Exercise 2.18
(define (reverse list-in)
  (define (reverse-iter remain partial)
    (if (null? remain)
        partial
        (reverse-iter (cdr remain) (cons (car remain) partial))))
  (reverse-iter list-in (list)))