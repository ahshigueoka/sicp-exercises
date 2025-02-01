#lang racket
(require rackunit "tree.rkt")
(require rackunit/text-ui)

(define test-square-tree
  (test-suite
   "Test for tree.rkt"
   (test-case
    "Test squaring a tree"
    (let (
          [example_1 (list 1
                           (list 2 (list 3 4) 5)
                           (list 6 7))]
          [expected_1 (list 1
                            (list 4 (list 9 16) 25)
                            (list 36 49))])
      (check-equal? (square-tree example_1) expected_1 "Square tree")))
   (test-case
    "Test squaring a tree without map"
    (let (
          [example_1 (list 1
                           (list 2 (list 3 4) 5)
                           (list 6 7))]
          [expected_1 (list 1
                            (list 4 (list 9 16) 25)
                            (list 36 49))])
      (check-equal? (square-tree-nomap example_1) expected_1 "Square tree")))
   (test-case
    "Test tree map with square"
    (let (
          [example_1 (list 1
                           (list 2 (list 3 4) 5)
                           (list 6 7))]
          [expected_1 (list 1
                            (list 4 (list 9 16) 25)
                            (list 36 49))])
      (check-equal? (tree-map (lambda (x) (* x x)) example_1) expected_1 "Map square to tree")))
   (test-case
    "Test tree map with scale"
    (let (
          [example_1 (list 1
                           (list 2 (list 3 4) 5)
                           (list 6 7))]
          [expected_1 (list 3
                            (list 6 (list 9 12) 15)
                            (list 18 21))])
      (check-equal? (tree-map (lambda (x) (* 3 x)) example_1) expected_1 "Map scale to tree")))))

(run-tests test-square-tree)