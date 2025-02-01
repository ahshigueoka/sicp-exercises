#lang racket
(require math/number-theory)


(define (flatmap proc seq)
  (foldr append null (map proc seq)))


(define (unique-triples n)
  (flatmap (lambda (i)
         (flatmap (lambda (j)
                (map (lambda (k) (list i j k))
                     (range 1 j)))
                (range 1 i)))
       (range 1 (+ n 1))))


(define (triple-sum upper-bound total)
  (filter (lambda (triple) (= total (apply + triple)))
          (unique-triples upper-bound)))


(provide flatmap
         unique-triples
         triple-sum)