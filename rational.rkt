#lang racket

; Compute the Greatest Common Divisor between `a` and `b`
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Make a pair that represents a rational number
; The first element is the numerator and the
; second element is the denominator.
; n: int
; d: int, not zero
(define (make-rat num den)
  (let ([cd (gcd (abs num) (abs den))])
    (cond [(zero? den) (error "Attempted to create a rational number with zero denominator")]
          [(negative? den) (cons (/ (- num) cd) (/ (- den) cd))]
          [else (cons (/ num cd) (/ den cd))])))

(define (print-rat x)
  (begin
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))