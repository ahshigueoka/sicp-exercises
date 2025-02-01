#lang racket
(require math/number-theory)


(define (flatmap proc seq)
  (foldr append null (map proc seq)))


(define (prime-sum? seq)
  (prime? (apply + seq)))


(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (unique-pairs n)
  (flatmap (lambda (value)
             (map (lambda (x) (list value x)) (range 1 value)))
           (range 1 (+ n 1))))


(provide flatmap
         prime-sum?
         make-pair-sum
         prime-sum-pairs
         unique-pairs)