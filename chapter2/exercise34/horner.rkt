#lang racket

; A more space-efficient implementation would use foldl,
; but it requires to reverse how the problem specified the
; coefficient order
(define (horner-eval x coefficient-sequence)
  (foldr (lambda (value acc) (+ (* acc x) value)) 0 coefficient-sequence))

(provide horner-eval)