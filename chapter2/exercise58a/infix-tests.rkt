#lang racket/base
(require rackunit
         rackunit/text-ui
         "infix.rkt")


(define symbolic-tests
  (test-suite
   "Test basic algebraic features"
   (test-case
    "Test for symbols"
    (check-true (variable? 'x))
    (check-false (variable? 1)))
   (test-case
    "Test for variable equality"
    (check-true (same-variable? 'x 'x))
    (check-false (same-variable? 'x 'z)))
   (test-case
    "Check if expression is equal to a number"
    (check-true (=number? 100 100))
    (check-true (=number? '100 100))
    (check-false (=number? 1 100))
    (check-false (=number? '1 100)))))

(define sum-tests
  (test-suite
   "Test infix sum operation"
   (test-case
    "Sum of numbers"
    (check-equal? (make-sum 2 3) 5))
   (test-case
    "Simplification with 0"
    (check-equal? (make-sum 'x 0) 'x))
   (test-case
    "Sum of symbols"
    (check-equal? (make-sum 'x 'y) '(x + y)))))


(define prod-tests
  (test-suite
   "Test infix prod operation"
   (test-case
    "Product of numbers"
    (check-equal? (make-product 2 3) 6))
   (test-case
    "Simplification with 0"
    (check-equal? (make-product 'x 0) 0))
   (test-case
    "Simplification with 1"
    (check-equal? (make-product 'x 1) 'x))
   (test-case
    "Product of symbols"
    (check-equal? (make-product 'x 'y) '(x * y)))))


(define exp-tests
  (test-suite
   "Test infix exp operation"
   (test-case
    "exponent of numbers"
    (check-equal? (make-exponentiation 2 3) 8))
   (test-case
    "Simplification with exponent 0"
    (check-equal? (make-exponentiation 3 0) 1))
   (test-case
    "Simplification with base 0"
    (check-equal? (make-exponentiation 0 4) 0))
   (test-case
    "Exponent of symbols"
    (check-equal? (make-exponentiation 'x 'y) '(x ** y)))
   (test-case
    "Exponent of mixed symbol and number"
    (check-equal? (make-exponentiation 2 'y) '(2 ** y)))))


(define derivation-tests
  (test-suite
   "Test symbolic exponentiation derivative"
   (test-case
    "Derive exponentiation"
    (check-equal? (deriv '(x ** 7) 'x) '(7 * (x ** 6))))
   (test-case
    "Derive exponentiation of linear term"
    (check-equal? (deriv '(x + 1) 'x) 1))
   (test-case
    "Derive mixed sum and product"
    (check-equal? (deriv '(x * (y * (x + 3))) 'x)
                  '((x * y) + (y * (x + 3)))))))


(run-tests symbolic-tests)
(run-tests sum-tests)
(run-tests exp-tests)
(run-tests derivation-tests)