#lang racket

(require rackunit "triple-sum.rkt")
(require rackunit/text-ui)


(define triple-sum-tests
  (test-suite
   "Test textbook examples for this exercise"
   (test-case
    "test flatmap"
    (check-equal? (flatmap range (list 1 2 3))
                  (list 0 0 1 0 1 2)
                  "Test flatmap on non-empty"))
   (test-case
    "test unique triples from {1, 2, 3}"
    (check-equal? (unique-triples 3)
                  (list (list 3 2 1))
                  "Unique triples with 1 2 3"))
   (test-case
    "test unique triples from {1, 2, 3, 4}"
    (check-equal? (unique-triples 4)
                  (list
                   (list 3 2 1)
                   (list 4 2 1)
                   (list 4 3 1)
                   (list 4 3 2))
                  "Unique triples with 1 2 3 4"))
   (test-case
    "test unique triples from {1, 2, 3, 4} that sum 7"
    (check-equal? (triple-sum 4 7)
                  (list (list 4 2 1))
                  "Unique triples with 1 2 3 4 that sum 7"))))


(run-tests triple-sum-tests)