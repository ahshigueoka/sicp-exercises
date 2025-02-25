#lang racket/base
(require rackunit
         rackunit/text-ui
         "differentiation.rkt")


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
   "Test sum operations"
   (test-case
    "Make sum of more than two terms"
    (check-equal? (make-sum 2 '(+ 3 4)) 9 "(+ 2 3 4)"))
   (test-case
    "Make sum of two sums"
    (check-equal? (make-sum '(+ 1 2) '(+ 3 4 5))
                  15
                  "(+ (+ 1 2 3) (+ 4 5))"))
   (test-case
    "Simplify expression with zero"
    (check-equal? (make-sum 1 0)
                  1
                  "(+ 1 0)"))
   (test-case
    "Simplify expression with several zeroes"
    (check-equal? (make-sum '(+ 1 0 3 0) '(+ 0 2 0 4))
                  10
                  "(+ (+ 1 0 3 0) (+ 0 2 0 4))"))
   (test-case
    "Simplify expression with numbers and symbols"
    (check-equal? (make-sum '(+ 1 x 3 0) '(+ 0 2 y 4))
                  '(+ 10 x y)
                  "(+ (+ 1 x 3 0) (+ 0 2 y 4)"))))


(define multiplication-tests
  (test-suite
   "Test muplication operations"
   (test-case
    "Make product of more than two terms"
    (check-equal? (make-product 2 '(* 3 4)) 24 "(* 2 3 4)"))
   (test-case
    "Make product of products"
    (check-equal? (make-product '(* 3 2) '(* -1 5)) -30))
   (test-case
    "Simplify expression with zero"
    (check-equal? (make-product 3 0) 0))
   (test-case
    "Simplify expression with one"
    (check-equal? (make-product 1 6) 6))
   (test-case
    "Simplify expression with several ones"
    (check-equal? (make-product '(* 1 2 3 2) '(* 1 2 1 4)) 96))
   (test-case
    "Simplify expression with numbers and symbols"
    (check-equal? (make-product '(* 1 x 3) '(* 2 y 4)) '(* 24 x y)))))

(define sum-prod-tests
  (test-suite
   "Test integration of sum and products"
   (test-case
    "Sum of products"
    (check-equal? (make-sum
                  (make-product 2 'x)
                  (make-product -1 4))
                  '(+ -4 (* 2 x))))
   (test-case
    "Product of sums"
    (check-equal? (make-product
                  (make-sum 2 'x)
                  (make-sum 'y 3))
                  '(* (+ 2 x) (+ 3 y))))))


(define diff-sum-tests
  (test-suite
   "Test differentiation of sums"
   (test-case
    "Derive sum of two terms"
    (check-equal? (deriv '(+ x 3) 'x) 1))
   (test-case
    "Derive sum of three terms"
    (check-equal? (deriv '(+ x y 3) 'x) 1))
   (test-case
    "Derive expression with several multiplications"
    (check-equal? (deriv (make-product '(* x y) '(+ x 3)) 'x)
                  '(+ (* x y) (* y (+ x 3)))))))


(run-tests symbolic-tests)
(run-tests sum-tests)
(run-tests multiplication-tests)
(run-tests sum-prod-tests)
(run-tests diff-sum-tests)