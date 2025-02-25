#lang racket/base


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list a1 '+ a2)]))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list m1 '* m2)]))

(define (make-exponentiation B N)
  (cond [(and (=number? B 0) (=number? N 0))
         (raise (make-exn:fail "0**0 is mathematically ill-defined"
                               (current-continuation-marks)))]
        [(=number? B 0) 0]
        [(=number? N 0) 1]
        [(=number? N 1) B]
        [(and (number? B) (number? N)) (expt B N)]
        [else (list B '** N)]))

(define (sum? expr)
  (and (list? expr) (eq? '+ (cadr expr))))

(define (addend expr)
  (car expr))

(define (augend expr)
  (caddr expr))

(define (product? expr)
  (and (list? expr) (eq? '* (cadr expr))))

(define (multiplier expr)
  (car expr))

(define (multiplicand expr)
  (caddr expr))

(define (exponentiation? expr)
  (and (list? expr) (eq? '** (cadr expr))))

(define (base expr)
  (car expr))

(define (exponent expr)
  (caddr expr))

(define (deriv expr var)
  (cond [(number? expr) 0]
        [(variable? expr)
         (if (same-variable? expr var) 1 0)]
        [(sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var))]
        [(product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr)))]
        [(exponentiation? expr)
         (let ([B (base expr)]
               [N (exponent expr)])
           (make-product (make-product N (make-exponentiation B (- N 1)))
                         (deriv B var)))]
        [else
         (error "unknown expression type -- DERIV" expr)]))


(provide variable?
         same-variable?
         =number?
         make-sum
         make-product
         make-exponentiation
         sum?
         addend
         augend
         product?
         multiplier
         multiplicand
         exponentiation?
         base
         exponent
         deriv)