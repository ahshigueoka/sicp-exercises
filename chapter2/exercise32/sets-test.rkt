#lang racket
(require rackunit "sets.rkt")
(require rackunit/text-ui)

(define test-subsets
  (test-suite
   "Test for sets.rkt"
   (test-case
    "Test the subsets of an empty set"
    (let (
          [example null]
          [expected (list null)])
      (check-equal? (subsets example) expected "Sets")))
   (test-case
    "Test subsets of (1 2 3)"
    (let (
          [example (list 1 2 3)]
          [expected (list
                     null
                     (list 3) (list 2) (list 2 3)
                     (list 1) (list 1 3) (list 1 2)
                     (list 1 2 3))])
      (check-equal? (subsets example) expected "Subsets of (1 2 3)")))))

(run-tests test-subsets)