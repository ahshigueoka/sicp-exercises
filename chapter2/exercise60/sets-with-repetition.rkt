#lang racket/base

; Exercise 2.60
;
; Since there is no worry to account for repetitions,
; adjoining is just a cons, executed in constant time
; and union just appending two lists, which has complexity O(n).
; The operation element-of-set? keeps O(n) complexity.
; Intersection keeps the complexity O(n^2)
; Because the elements are not unique, it is costly
; to calculate the set cardinality, since it will be necessary
; to remove all repetitions before counting.
; A way to remove repetition is checking for each element
; in the original set if it had already been included in the
; set with unique elements. Since element-of-set? is called for each
; element in the set, the complexity is O(n^2)
(define (unique-elements set)
  (define (get-unique partial remain)
    (cond [(null? remain) partial]
          [(element-of-set? (car remain) partial) (get-unique partial (cdr remain))]
          [else (get-unique (cons (car remain) partial) (cdr remain))]))
  (get-unique '() set))

(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))

(define (union-set set1 set2)
  (append set1 set2))

(provide element-of-set?
         adjoin-set
         intersection-set
         union-set
         unique-elements)