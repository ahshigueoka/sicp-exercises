#lang racket

(require rackunit "unique-pairs.rkt")
(require rackunit/text-ui)


(define unique-pairs-tests
  (test-suite
   "Test textbook examples for this exercise"
   (test-case
    "test flatmap"
    (check-equal? (flatmap range (list 1 2 3))
                  (list 0 0 1 0 1 2)
                  "Test flatmap on non-empty"))
   (test-case
    "test prime-sum?"
    (check-true (prime-sum? (list 2 3)) "2+3=5 should be prime")
    (check-true (prime-sum? (list 2 3 6)) "2+3+6=11 should be prime")
    (check-true (prime-sum? (list 3)) "3 should be prime")
    (check-true (prime-sum? (list 2 3)) "2+3 should be prime")
    (check-false (prime-sum? (list 7 11 17)) "7+11+17 should not be prime")
    (check-false (prime-sum? (list 0)) "0 should not be prime"))
   (test-case
    "create tuples from pair and respective sum"
    (check-equal? (make-pair-sum (list 4 9)) (list 4 9 13))
    (check-equal? (make-pair-sum (list 1 3 5)) (list 1 3 4))
    (check-equal? (make-pair-sum (list -1 9)) (list -1 9 8))
    (check-equal? (make-pair-sum (list -2 -3)) (list -2 -3 -5))
    (check-equal? (make-pair-sum (list 0 0)) (list 0 0 0)))
   (test-case
    "Find pairs of numbers whose sum is prime"
    (check-equal? (prime-sum-pairs 6)
                  (list
                   (list 2 1 3)
                   (list 3 2 5)
                   (list 4 1 5)
                   (list 4 3 7)
                   (list 5 2 7)
                   (list 6 1 7)
                   (list 6 5 11))
                  "Pairs whose sum is prime"))
   (test-case
    "Find distinct pairs of numbers up to n"
    (check-equal? (unique-pairs 2)
                  (list (list 2 1))
                  "Up to 2")
    (check-equal? (unique-pairs 3)
                  (list (list 2 1) (list 3 1) (list 3 2))
                  "Up to 3")
    (check-equal? (unique-pairs 4)
                  (list
                   (list 2 1)
                   (list 3 1)
                   (list 3 2)
                   (list 4 1)
                   (list 4 2)
                   (list 4 3))  "Up to 2"))))


(run-tests unique-pairs-tests)