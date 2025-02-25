#lang racket/base
(require rackunit
         rackunit/text-ui
         "bin-tree-sets.rkt")

; just a shorthand notation
(define ^ make-tree)
(define _ null)

(define bin-tree-tests
  (test-suite
   "Tests containment in tree"
   (test-case
    "A single node"
    (let ([set (list 3 null null)])
      (check-true (element-of-set? 3 set) "3 is in the set")
      (check-false (element-of-set? 4 set) "4 is not in the set")))
   (test-case
    "A balanced tree"
    (let ([set '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))])
      (check-true (element-of-set? 5 set) "Checking root")
      (check-true (element-of-set? 1 set) "Checking leaf")
      (check-true (element-of-set? 9 set) "Checking intermediate node")
      (check-false (element-of-set? 6 set) "Checking non existent value")))
   (test-case
    "Test tree constructor"
    (check-equal? (^ 5 (^ 3 (^ 1 _ _) _) (^ 9 (^ 7 _ _) (^ 11 _ _)))
                  '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))))))

(define adjoin-tree-tests
  (test-suite
   "Test adjoining in tree"
   (test-case
    "Test adjoining to an empty list"
    (check-equal? (adjoin-set 1 '()) '(1 () ())))
   (test-case
    "Test adjoining to a single node"
    (check-equal? (adjoin-set 4 (make-tree 3 '() '()))
                (^ 3 _ (^ 4 _ _))))
   (test-case
    "Test adjoining to a balanced tree"
    (check-equal? (adjoin-set 4 (^ 5 (^ 3 (^ 1 _ _) _) (^ 9 (^ 7 _ _) (^ 11 _ _))))
                  (^ 5 (^ 3 (^ 1 _ _) (^ 4 _ _)) (^ 9 (^ 7 _ _) (^ 11 _ _)))))))

(define tree-list-1-tests
  (test-suite
   "Test the conversions from tree to list"
   (test-case
    "Test on tree centered at 7"
    (check-equal? (tree->list-1 (^ 7 (^ 3 (^ 1 _ _) (^ 5 _ _)) (^ 9 _ (^ 11 _ _))))
                  '(1 3 5 7 9 11)))
   (test-case
    "Test on tree centered at 3"
    (check-equal? (tree->list-1 (^ 3 (^ 1 _ _) (^ 7 (^ 5 _ _) (^ 9 _ (^ 11 _ _)))))
                  '(1 3 5 7 9 11)))
   (test-case
    "Test on tree centered at 5"
    (check-equal? (tree->list-1 (^ 5 (^ 3 (^ 1 _ _) _) (^ 9 (^ 7 _ _) (^ 11 _ _))))
                  '(1 3 5 7 9 11)))))


(define tree-list-2-tests
  (test-suite
   "Test the conversions from tree to list"
   (test-case
    "Test on tree centered at 7"
    (check-equal? (tree->list-2 (^ 7 (^ 3 (^ 1 _ _) (^ 5 _ _)) (^ 9 _ (^ 11 _ _))))
                  '(1 3 5 7 9 11)))
   (test-case
    "Test on tree centered at 3"
    (check-equal? (tree->list-2 (^ 3 (^ 1 _ _) (^ 7 (^ 5 _ _) (^ 9 _ (^ 11 _ _)))))
                  '(1 3 5 7 9 11)))
   (test-case
    "Test on tree centered at 5"
    (check-equal? (tree->list-2 (^ 5 (^ 3 (^ 1 _ _) _) (^ 9 (^ 7 _ _) (^ 11 _ _))))
                  '(1 3 5 7 9 11)))))

(define balanced-tree-tests
  (test-suite
   "Test conversion from list to balanced tree"
   (test-case
    "Test on the exercise list"
    (check-equal? (list->tree '(1 3 5 7 9 11))
                              (^ 5 (^ 1 _ (^ 3 _ _)) (^ 9 (^ 7 _ _) (^ 11 _ _)))))))


(define union-tree-tests
  (test-suite
   "Test union of balanced trees"
   (test-case
    "Union of two small trees"
    (check-equal? (union-set (^ 3 (^ 1 _ _) (^ 5 _ _)) (^ 7 (^ 6 _ _) (^ 8 _ _)))
                  (list->tree '(1 3 5 6 7 8))))))


(define intersection-tree-tests
  (test-suite
   "Test intersection of balanced trees"
   (test-case
    "Intersection of two small trees"
    (check-equal? (intersection-set
                   (^ 6 (^ 1 _ (^ 3 _ _)) (^ 8 _ (^ 9 _ _)))
                   (^ 4 (^ 2 _ (^ 3 _ _)) (^ 7 (^ 6 _ _) (^ 9 _ _))))
                  (list->tree '(3 6 9))))))


(run-tests bin-tree-tests)
(run-tests adjoin-tree-tests)
(run-tests tree-list-1-tests)
(run-tests tree-list-2-tests)
(run-tests balanced-tree-tests)
(run-tests union-tree-tests)
