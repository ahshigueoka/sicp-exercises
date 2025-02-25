#lang racket/base
(require rackunit
         rackunit/text-ui
         "symbolic.rkt")


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

(define exponentiation-tests
  (test-suite
   "Test symbolic exponentiation"
   (test-case
    "Test if expression is exponentiation"
    (check-true (exponentiation? '(** 2 3)))
    (check-false (exponentiation? '(* 2 3)))
    (check-false (exponentiation? '(+ 2 3))))
   (test-case
    "Test base extraction"
    (check-equal? (base '(** x 3)) 'x)
    (check-equal? (base '(** 3 2)) 3))
   (test-case
    "Test exponent extraction"
    (check-equal? (exponent '(** x 3)) 3)
    (check-equal? (exponent '(** 3 2)) 2))
   (test-case
    "Test exponent constructor"
    (check-equal? (make-exponentiation 'x 4) '(** x 4))
    (check-equal? (make-exponentiation 2 3) '(** 2 3)))
   (test-case
    "Test exponentiation of ill-defined operation"
    (check-exn exn:fail?
               (lambda ()
                 (make-exponentiation 0 0))))
   (test-case
    "Test 0 power to a non-null number"
    (check-equal? (make-exponentiation 0 3)  0 "0**3 == 0")
    (check-equal? (make-exponentiation 0 -1) 0 "0**-1 == 0")
    (check-equal? (make-exponentiation 0 'x) 0 "0**x == 0 (assuming x != 0)"))
   (test-case
    "Test 0 power to a non-null number"
    (check-equal? (make-exponentiation  5 0) 1 "5**0 == 1")
    (check-equal? (make-exponentiation -9 0) 1 "-9**0 == 1")
    (check-equal? (make-exponentiation 'x 0) 1 "x**0 == 1 (assuming x != 0)"))
   (test-case
    "Base extraction"
    (check-equal? (base (make-exponentiation  2 3)) 2 "base of 2^3")
    (check-equal? (base (make-exponentiation 'x 2)) 'x "base of x^2")
    (check-equal? (base (make-exponentiation  'x 'y)) 'x "base of x^y"))
   (test-case
    "Exponent extraction"
    (check-equal? (exponent (make-exponentiation  2 3)) 3 "exponent of 2^3")
    (check-equal? (exponent (make-exponentiation 'x 2)) 2 "exponent of x^2")
    (check-equal? (exponent (make-exponentiation  'x 'y)) 'y "exponent of x^y"))))
    

(define derivation-tests
  (test-suite
   "Test symbolic exponentiation"
   (test-case
    "Derive exponentiation"
    (check-equal? (deriv '(** x 7) 'x) '(* 7 (** x 6))))
   (test-case
    "Derive exponentiation of linear term"
    (check-equal? (deriv '(** x 1) 'x) 1))))

(run-tests symbolic-tests)
(run-tests exponentiation-tests)
(run-tests derivation-tests)

curl -LO https://github.com/kubernetes/minikube/releases/latest/download/minikube-linux-amd64
sudo install minikube-linux-amd64 /usr/local/bin/minikube && rm minikube-linux-amd64

