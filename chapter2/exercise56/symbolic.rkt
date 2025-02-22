#lang racket/base


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list '+ a1 a2)]))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list '* m1 m2)]))

(define (sum? expr)
  (and (list? expr) (eq? '+ (car expr))))

(define (addend expr)
  (cadr expr))

(define (augend expr)
  (caddr expr))

(define (product? expr)
  (and (list? expr) (eq? '* (car expr))))

(define (multiplier expr)
  (cadr expr))

(define (multiplicand expr)
  (caddr expr))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp)))]
        [else
         (error "unknown expression type -- DERIV" exp)]))


(provide variable?
         same-variable?
         make-sum
         make-product
         sum?
         addend
         augend
         product?
         multiplier
         multiplicand
         deriv)