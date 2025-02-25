#lang racket/base

; Exercise 2.66
; For this exercise, it will be necessary change how
; the ordering routines make sorts, since
; each node will be a pair of key and value
;
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (lookup given-key set-of-records)
  (cond [(null? sef-of-records) #f]
        [(< given-key (key (entry tree))) (lookup given-key (left-branch tree))]
        [(> given-key (key (entry tree))) (lookup given-key (right-branch tree))]
        [else (entry tree)]))