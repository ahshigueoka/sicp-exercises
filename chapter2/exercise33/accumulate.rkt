#lang racket


(define (custom-map transform sequence)
  (foldr (lambda (x y) (cons (transform x) y))
         null
         sequence))

(define (custom-append seq1 seq2)
  (foldr cons seq2 seq1))

(define (custom-length sequence)
  (foldr (lambda (x count) (+ count 1)) 0 sequence))


(provide custom-map
         custom-append
         custom-length)