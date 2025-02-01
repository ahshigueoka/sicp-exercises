#lang racket
(require rackunit
         rackunit/text-ui
         "queens.rkt")

(define queens-tests
  (test-suite
   "Test routines from queen placement problem"
   (test-case
    "Test case for the empty board"
    (check-equal? empty-board null "Empty board representation"))
   (test-case
    "Adjoin to empty list"
    (check-equal? (adjoin-position 1 empty-board)
                  (list 1)
                  "Adding placing at row 1 to empty board")
    (check-equal? (adjoin-position 2 empty-board)
                  (list 2)
                  "Adding placing at row 2 to empty board")
    (check-equal? (adjoin-position 3 empty-board)
                  (list 3)
                  "Adding placing at row 3 to empty board")
   (test-case
    "Adjoin a queen at position 2 to placement (7 3)"
    (check-equal? (adjoin-position 2 (list 7 3))
                  (list 2 7 3)
                  "Adding to a row"))
   (test-case
    "Adjoin a queen at position 8 to placements (2 7 3)"
    (check-equal? (adjoin-position 8 (list 2 7 3))
                  (list 8 2 7 3)
                  "Adding to a row"))
   (test-case
    "Test case for the empty board"
    (check-equal? empty-board null "Empty board representation"))
   (test-case
    "Safe for no placed queen"
    (check-equal? (safe? 1 empty-board) true "Checking empty board")
    (check-equal? (safe? 2 empty-board) true "Checking empty board"))
   (test-case
    "Check safety for 1 placed queen"
    (check-equal? (safe? 1 (list 2)) true "Checking row 2")
    (check-equal? (safe? 1 (list 3)) true "Checking row 3")
    (check-equal? (safe? 1 (list 4)) true "Checking row 4")
    (check-equal? (safe? 1 (list 8)) true "Checking row 8"))
   (test-case
    "Check safety for 2 placed queens"
    (check-equal? (safe? 2 (list 3 7)) true "Checking 3 7")
    (check-equal? (safe? 2 (list 1 8)) true "Checking 1 8")
    (check-equal? (safe? 2 (list 3 5)) true "Checking 3 5")
    (check-equal? (safe? 2 (list 8 4)) true "Checking 8 4")
    (check-equal? (safe? 2 (list 1 2)) false "Checking 1 2")
    (check-equal? (safe? 2 (list 4 5)) false "Checking 4 5")
    (check-equal? (safe? 2 (list 6 6)) false "Checking 6 6")
    (check-equal? (safe? 2 (list 7 6)) false "Checking 7 6"))
   (test-case
    "Check safety for 3 placed queens"
    (check-equal? (safe? 3 (list 3 7 2)) true "Valid: 3 7 2")
    (check-equal? (safe? 3 (list 2 8 5)) true "Valid: 2 8 5")
    (check-equal? (safe? 3 (list 1 4 6)) true "Valid: 1 4 6")
    (check-equal? (safe? 3 (list 5 1 4)) true "Valid: 5 1 4")
    (check-equal? (safe? 3 (list 1 4 3)) false "Invalid: 1 4 3")
    (check-equal? (safe? 3 (list 2 6 2)) false "Invalid: 2 6 2")
    (check-equal? (safe? 3 (list 1 1 6)) false "Invalid: 1 6 6")
    (check-equal? (safe? 3 (list 3 6 5)) false "Invalid: 3 6 5"))
   (test-case
    "Place no queen on a board of any size"
    (check-equal? (queen-cols 0 8) (list empty-board) "Placing no queen"))
   (test-case
    "Place one queen on a board of size 4"
    (check-equal? (queen-cols 1 4)
                  (list (list 1)
                        (list 2)
                        (list 3)
                        (list 4)) "Placing one queen"))
   (test-case
    "Place two queens on a board of size 3"
    (check-equal? (queen-cols 2 3)
                  (list (list 3 1)
                        (list 1 3)) "Placing two queens queen")))))


(run-tests queens-tests)