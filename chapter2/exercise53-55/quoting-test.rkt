#lang racket/base
(require rackunit
         rackunit/text-ui
         "quoting.rkt")


(define quoting-tests
  (test-suite
   "Test recursive equality procedure"
   (test-case
    "Test equal symbols"
    (check-true (recursive-eq 'x 'x) "'x is equal to 'x"))
   (test-case
    "Test different symbols"
    (check-false (recursive-eq 'x 'y) "'x and 'y are different"))
   (test-case
    "Test empty lists"
    (check-true (recursive-eq '() '()) "'() are the same"))
   (test-case
    "Test one empty and other a symbola"
    (check-false (recursive-eq '() 'x) "'() and 'x are not the save"))
   (test-case
    "Test one a symbol and the other an empty list"
    (check-false (recursive-eq 'x '()) "''x and '() are not the same"))
   (test-case
    "Two equal flat lists"
    (check-true (recursive-eq '(x y) '(x y)) "'(x y) for both"))
   (test-case
    "Test different lists"
    (check-false (recursive-eq '(x1 x2) '(y1 y2)) "'(x1 xy1) and '(x2 y2) are different"))
   (test-case
    "Test lists of different length"
    (check-false (recursive-eq '(x y z) '(x y)) "'(x y z) and '(x y) are different"))
   (test-case
    "Test one empty list and the other is not"
    (check-false (recursive-eq '() '(x y)) "'() and '(x y) are not the same"))
   (test-case
    "Test nested list equality"
    (check-true (recursive-eq '(w (x y) z) '(w (x y) z)) "both lists are '(w (x y) z)"))
   (test-case
    "Test nested different lists"
    (check-false (recursive-eq '(w (x y) z) '(w (y x) z)) "'inner list is different"))))


(run-tests quoting-tests)