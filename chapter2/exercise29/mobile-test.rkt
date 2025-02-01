#lang racket
(require rackunit "mobile.rkt")
(require rackunit/text-ui)

(define test-mobile
  (test-suite
   "Tests for mobile.rkt"
   (check-equal? (make-branch 10 4) (list 10 4) "Branch creation")
   (test-case
    "Test with a small tree with two branches"
    (let* (
          [b1 (list 1 1)]
          [b2 (list 1 2)]
          [b3 (list 2 3)]
          [b4 (list 3 2)]
          [m1 (list b1 b2)]
          [m2 (list b3 b4)])
      (check-equal? (make-mobile b1 b2) m1 "Small tree")
      (check-equal? (left-branch m1) b1 "Left branch")
      (check-equal? (right-branch m1) b2 "Right branch")
      (check-equal? (total-weight b1) 1 "Weight of a branch")
      (check-equal? (total-weight m1) 3 "Weight of small tree")
      (check-equal? (balanced? b1) true "Branches are always balanced")
      (check-equal? (balanced? m1) false "Unbalanced node")
      (check-equal? (balanced? m2) true "Balanced node")))
   (test-case
    "Test on a larger tree"
    (let* ([al (list 1 1)]
           [ar (list 1 3)]
           [dl (list 2 2)]
           [dr (list 0 2)]
           [ma (list al ar)]
           [mb 4]
           [cl (list 1 ma)]
           [cr (list 1 mb)]
           [mc (list cl cr)]
           [md (list dl dr)]
           [el (list 1 mc)]
           [er (list 1 md)]
           [me (list el er)])
      (check-equal? (make-mobile el er) me "Large tree")
      (check-equal? (left-branch me) el "Left branch")
      (check-equal? (right-branch me) er "Right branch")
      (check-equal? (total-weight ma) 4 "Weight of mobile a")
      (check-equal? (total-weight mb) 4 "Weight of mobile b")
      (check-equal? (total-weight mc) 8 "Weight of mobile c")
      (check-equal? (total-weight md) 4 "Weight of mobile d")
      (check-equal? (total-weight me) 12 "Weight of mobile e")))
   (test-case
    "Tests on a balanced tree"
    (let* ([al (make-branch 1 2)]
           [ar (make-branch 2 1)]
           [bl (make-branch 3 2)]
           [br (make-branch 2 3)]
           [ma (make-mobile al ar)]
           [mb (make-mobile bl br)]
           [cl (make-branch 5 ma)]
           [cr (make-branch 3 mb)]
           [mc (make-mobile cl cr)])
      (check-equal? (balanced? ma) true "Balanced node a")
      (check-equal? (balanced? mb) true "Balanced node b")
      (check-equal? (balanced? mc) true "Balanced node c")
      (check-equal? (balanced-tree? mc) true "Balanced tree")))
   (test-case
    "Tests on an unbalanced tree"
    (let* ([al (make-branch 1 2)]
           [ar (make-branch 2 1)]
           [bl (make-branch 1 3)]
           [br (make-branch 4 1)]
           [ma (make-mobile al ar)]
           [mb (make-mobile bl br)]
           [cl (make-branch 4 ma)]
           [cr (make-branch 3 mb)]
           [mc (make-mobile cl cr)])
      (check-equal? (balanced? ma) true "Balanced node a")
      (check-equal? (balanced? mb) false "Unbalanced node b")
      (check-equal? (balanced? mc) true "Balanced node c")
      (check-equal? (balanced-tree? mc) false "Unbalanced node c")))))

(run-tests test-mobile)