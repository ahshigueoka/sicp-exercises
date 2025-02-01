#lang racket

(define (dot-product v w)
  (foldl + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define (transpose m)
  (apply map list m))

(provide dot-product
         matrix-*-vector
         matrix-*-matrix
         transpose)