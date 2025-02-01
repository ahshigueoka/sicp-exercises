#lang racket

(require rackunit "reverse-seq.rkt")
(require rackunit/text-ui)


(define reverse-seq-tests
  (test-suite
   "Tests custom list reversal methods"
   (test-case
    "test folding right on empty list"
      (check-equal? (reverse-r null) null "Reversal of empty list"))
   (test-case
    "test folding right on 1-element list"
      (check-equal? (reverse-r (list 1)) (list 1) "Reversal of 1-element list"))
   (test-case
    "test folding right on 3-element list"
      (check-equal? (reverse-r (list 1 2 3)) (list 3 2 1) "Reversal of 3-element list"))
   (test-case
    "test folding left on empty list"
      (check-equal? (reverse-l null) null "Reversal of empty list"))
   (test-case
    "test folding left on 1-element list"
      (check-equal? (reverse-l (list 1)) (list 1) "Reversal of 1-element list"))
   (test-case
    "test folding left on 3-element list"
      (check-equal? (reverse-l (list 1 2 3)) (list 3 2 1) "Reversal of 3-element list"))))


(run-tests reverse-seq-tests)