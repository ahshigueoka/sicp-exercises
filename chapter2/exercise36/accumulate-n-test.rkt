#lang racket

(require rackunit "accumulate-n.rkt")
(require rackunit/text-ui)


(define accumulate-n-tests
  (test-suite
   "Tests for accumulate-n"
   (test-case
    "text book example"
    (let ([data_in (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))]
          [expected (list 22 26 30)])
      (check-equal? (accumulate-n + 0 data_in) expected "Textbook example")))))


(run-tests accumulate-n-tests)