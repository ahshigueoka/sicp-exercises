#lang racket
; Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (sub) (cons (car s) sub)) rest)))))

; Line 7 computes all the subsets of "s", except for the first element
; Then line 8 computes all the subsets of "s" using the reasoning:
;     - It should consist of the union all the subsets excluding the
;       first element, which is represented by "rest"; plus
;     - It should also consist of the union of all the subsets including
;       the first element, which can be found including the first element,
;       (car s), to each subset in "s"

(provide subsets)