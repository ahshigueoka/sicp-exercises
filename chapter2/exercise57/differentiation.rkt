#lang racket/base

(require racket/list)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (collect-sum parcels)
  (let ([number-sum (apply + (filter number? parcels))]
        [non-numeric (filter (lambda (val) (not (number? val)))
                             parcels)])
    (if (= number-sum 0)
        non-numeric
        (cons number-sum non-numeric))))

(define (collect-prod parcels)
  (let ([number-prod (apply * (filter number? parcels))]
        [non-numeric (filter (lambda (val) (not (number? val)))
                             parcels)])
    (cond [(= number-prod 0) '(0)]
         [(= number-prod 1) non-numeric]
         [else (cons number-prod non-numeric)])))

(define (make-sum left right)
  (let* ([left-parcels (if (sum? left)
                           (cdr left)
                           (list left))]
         [right-parcels (if (sum? right)
                            (cdr right)
                            (list right))]
         [simplified (collect-sum (append left-parcels right-parcels))])
    (if (= (length simplified) 1)
        (car simplified)
        (cons '+ simplified))))

(define (make-product left right)
  (let* ([left-factors (if (product? left)
                           (cdr left)
                           (list left))]
         [right-factors (if (product? right)
                            (cdr right)
                            (list right))]
         [simplified (collect-prod (append left-factors right-factors))])
    (if (= (length simplified) 1)
        (car simplified)
        (cons '* simplified))))

(define (make-exponentiation B N)
  (cond [(and (=number? B 0) (=number? N 0))
         (raise (make-exn:fail "0**0 is mathematically ill-defined"
                               (current-continuation-marks)))]
        [(=number? B 0) 0]
        [(=number? N 0) 1]
        [(=number? N 1) B]
        [else (list '** B N)]))

(define (sum? expr)
  (and (list? expr) (eq? '+ (car expr))))

(define (addend expr)
  (cadr expr))

(define (augend expr)
  (if (= (length expr) 3)
      (caddr expr)
      (cons '+ (cddr expr))))

(define (product? expr)
  (and (list? expr) (eq? '* (car expr))))

(define (multiplier expr)
  (cadr expr))

(define (multiplicand expr)
  (if (= (length expr) 3)
      (caddr expr)
      (cons '* (cddr expr))))

(define (exponentiation? expr)
  (and (list? expr) (eq? '** (car expr))))

(define (base expr)
  (cadr expr))

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