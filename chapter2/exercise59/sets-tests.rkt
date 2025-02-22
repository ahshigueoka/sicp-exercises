#lang racket/base

(require rackunit
         rackunit/text-ui
         "sets.rkt")

(define sets-tests
  (test-suite
   "Tests for set operations"
   (test-case
    "Check inclusion in empty set"
    (check-false (element-of-set? 1 '()) "1 in empty set?"))
   (test-case
    "Check non presence in non-empty set"
    (check-false (element-of-set? 1 '( 2 3 4)) "1 in (2, 3, 4)?"))
   (test-case
    "Check presence in non-empty set"
    (check-true (element-of-set? 1 '(1 2 3 4)) "1 in (1, 2, 3, 4)?"))
   (test-case
    "Add an element to an empty set"
    (check-equal? (adjoin-set 'x '()) '(x) "Adjoining to empty set"))
   (test-case
    "Add an element to a non-empty set"
    (check-equal? (adjoin-set 'x '(y z)) '(x y z) "Adjoining to non-empty set"))
   (test-case
    "Add an element to a set with existing element"
    (check-equal? (adjoin-set 'x '(z x y)) '(z x y) "Adjoining to a set that has x"))
   (test-case
    "Empty intersection"
    (check-equal? (intersection-set '(1 2 3) '(4 5 6)) '() "Empty intersection"))
   (test-case
    "Non-emtpy intersection"
    (check-equal? (intersection-set '(1 2 3) '(3 5 1 4)) '(1 3) "Non empty intersection"))
   (test-case
    "Union with empty set"
    (check-equal? (union-set '(1 2 3) '()) '(1 2 3) "Empty union"))
   (test-case
    "Union of non-empty, non intersecting sets"
    (check-equal? (union-set '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6) "Non-intersecting sets"))
   (test-case
    "Union of intersecting sets"
    (check-equal? (union-set '(3 9 5) '(4 9 0 3)) '(5 4 9 0 3) "Intersecting sets"))))

(run-tests sets-tests)