#lang racket

(require rackunit "horner.rkt")
(require rackunit/text-ui)


(define horner-tests
  (test-suite
   "Tests for horner.rkt"
   (test-case
    "Null polynomial"
    (check-within (horner-eval 0 (list 0)) 0 1e-6)
    (check-within (horner-eval 3 (list 0)) 0 1e-6)
    (check-within (horner-eval 0 (list 0 0 0)) 0 1e-6)
    (check-within (horner-eval 3 (list 0 0 0)) 0 1e-6))
   (test-case
    "Constant non-null polynomial"
    (check-within (horner-eval 0 (list 5)) 5 1e-6)
    (check-within (horner-eval 2 (list -8)) -8 1e-6)
    (check-within (horner-eval -3 (list 5 0)) 5 1e-6)
    (check-within (horner-eval 0.5 (list -8 0 0)) -8 1e-6))
   (test-case
    "1st degree polynomial"
    (check-within (horner-eval 5 (list 0 1)) 5 1e-6)
    (check-within (horner-eval -2 (list -1 4)) -9 1e-6)
    (check-within (horner-eval 3 (list -3 4)) 9 1e-6)
    (check-within (horner-eval 3 (list -3 4 0)) 9 1e-6))
   (test-case
    "5th degree polynomial"
    (check-within (horner-eval 0 (list 1 3 0 5 0 1)) 1 1e-6)
    (check-within (horner-eval 2 (list 1 3 0 5 0 1)) 79 1e-6)
    (check-within (horner-eval -3 (list 1 3 0 5 0 1)) -386 1e-6)
    (check-within (horner-eval 0.5 (list 1 3 0 5 0 1)) 3.15625 1e-6))))


(run-tests horner-tests)