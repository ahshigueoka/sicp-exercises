#lang racket/base

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(< x (entry set)) (element-of-set? x (left-branch set))]
        [(> x (entry set)) (element-of-set? x (right-branch set))]
        [else #t]))


(define (adjoin-set x set)
  (cond [(null? set) (make-tree x '() '())]
        [(< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set))]
        [(> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set)))]
        [else set]))


; Exercise 2.63
; Both procedures below yield the same result, as can be seen
; in the unit tests in bin-tree-sets-tests.rkt
; Both recurse once for each node in the tree, so they
; have the same complexity.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


; Exercise 2.64
; The function below accepts an ordered list elts
; and creates a balanced tree from the first n elements
; of elts. This is accomplished by always
; creating a new entry point from the middle
; of the list, recursively.
; The algorithm executes at each node 3 cars, 3 cdrs and 1 cons
; in a recursive process of height log n
;
; The total number of operations are 
; 1
; :
; n/4
; n/2
; n
; times 3
; thus, 6n cars, 6n cdrs and 6n cons.
; The total is a O(n) process
; It is not O(n logn) because the algorithm
; does not run over the list to find pivots
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ([left-size (quotient (- n 1) 2)]
             [left-result (partial-tree elts left-size)]
             [left-tree (car left-result)]
             [non-left-elts (cdr left-result)]
             [right-size (- n (+ left-size 1))]
             [this-entry (car non-left-elts)]
             [right-result (partial-tree (cdr non-left-elts) right-size)]
             [right-tree (car right-result)]
             [remaining-elts (cdr right-result)])
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; Exercise 2.65
;
; For this assignment, we will reuse the intersection
; and union of ordered lists from the previous section
; Then plumb together the routines to convert binary trees
; to and from lists
(define (intersection-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([min1 (car set1)]
            [min2 (car set2)])
        (cond [(< min1 min2) (intersection-list (cdr set1) set2)]
              [(> min1 min2) (intersection-list set1 (cdr set2))]
              [else (cons min1 (intersection-list (cdr set1) (cdr set2)))]))))


(define (union-list set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else (let ([min1 (car set1)]
                    [min2 (car set2)])
                (cond [(< min1 min2) (cons min1 (union-list (cdr set1) set2))]
                      [(> min1 min2) (cons min2 (union-list set1 (cdr set2)))]
                      [else (cons min1 (union-list (cdr set1) (cdr set2)))]))]))


(define (union-set set1 set2)
  (list->tree
   (union-list
    (tree->list-2 set1)
    (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (list->tree
   (intersection-list
    (tree->list-2 set1)
    (tree->list-2 set2))))


(provide entry
         left-branch
         right-branch
         make-tree
         adjoin-set
         element-of-set?
         tree->list-1
         tree->list-2
         list->tree
         union-set
         intersection-set)