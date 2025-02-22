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
    (check-false (variable? 1)))))
    


(run-tests symbolic-tests)