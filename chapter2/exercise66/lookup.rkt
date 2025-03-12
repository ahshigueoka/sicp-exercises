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

(define (make-record key value)
  (cons key value))

(define (key record)
  (car record))

(define (value record)
  (cdr record))

(define (lookup given-key set-of-records)
  (cond [(null? set-of-records) (cons given-key null)]
        [(< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records))]
        [(> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records))]
        [else (entry set-of-records)]))

(provide lookup
         make-record
         list->tree)