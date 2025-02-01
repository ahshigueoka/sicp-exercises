#lang racket

(require rackunit "count-leaves.rkt")
(require rackunit/text-ui)


(define count-leaves-tests
  (test-suite
   "Tests for count-leaves"
   (test-case
    "4 leaves"
    (let ([tree4 (cons (list 1 2) (list 3 4))])
      (check-equal? (count-leaves tree4) 4 "Tree with 4 leaves")))
   (test-case
    "8 leaves"
    (let* ([tree4 (cons (list 1 2) (list 3 4))]
           [tree8 (list tree4 tree4)])
      (check-equal? (count-leaves tree8) 8 "Tree with 8 leaves")))
   (test-case
    "15 leaves"
    (let ([tree11 (list (list 1 4 (list 8 9 10)) (list (list 11 13) 19 (list 20 22) 23))])
      (check-equal? (count-leaves tree11) 11 "Tree with 11 leaves")))))


(run-tests count-leaves-tests)