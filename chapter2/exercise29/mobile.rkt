#lang racket
; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (weight-moment branch)
  (* (branch-length branch)
     (total-weight branch)))

(define (is-branch? mobile)
  (not (list? (car mobile))))

(define (is-leaf? mobile)
  (not (list? mobile)))

(define (total-weight mobile)
  (cond [(is-leaf? mobile) mobile]
        [(is-branch? mobile) (total-weight (car (cdr mobile)))]
        [else (+ (total-weight (left-branch mobile))
                 (total-weight (right-branch mobile)))]))

(define (balanced? mobile)
  (cond [(is-leaf? mobile) true]
        [(is-branch? mobile) true]
        [else (= (weight-moment (left-branch mobile))
                 (weight-moment (right-branch mobile)))]))

(define (balanced-tree? mobile)
  (if (is-leaf? mobile)
      true
      (and (balanced? mobile)
           (balanced-tree? (left-branch mobile))
           (balanced-tree? (right-branch mobile)))))


(provide make-mobile
         make-branch
         left-branch
         right-branch
         total-weight
         balanced?
         balanced-tree?)