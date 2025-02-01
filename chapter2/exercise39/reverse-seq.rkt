#lang racket

(define (reverse-r sequence)
  (foldr (lambda (x y) (append y (list x))) null sequence))

(define (reverse-l sequence)
  (foldl (lambda (x y) (cons x y)) null sequence))

(provide reverse-r
         reverse-l)