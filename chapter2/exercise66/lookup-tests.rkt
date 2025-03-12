#lang racket/base
(require rackunit
         rackunit/text-ui
         "lookup.rkt")


(define lookup-tests
  (test-suite
   "Test the lookup procedure in binary trees"
   (test-case
    "Test in a balanced binary tree"
    (let ([tree (list->tree (list (cons 1 'a)
                                  (cons 3 'c)
                                  (cons 5 'e)
                                  (cons 7 'g)
                                  (cons 9 'i)
                                  (cons 11 'k)))])
      (check-equal? (lookup 1 tree) (make-record 1 'a) "Checking record 1")
      (check-equal? (lookup 5 tree) (make-record 5 'e) "Checking record 5")
      (check-equal? (lookup 9 tree) (make-record 9 'i) "Checking record 9")
      (check-equal? (lookup 4 tree) (make-record 4 null) "Checking record 4")))))


(run-tests lookup-tests)