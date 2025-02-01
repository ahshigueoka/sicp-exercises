#lang racket

(require rackunit "basic-linalg.rkt")
(require rackunit/text-ui)


(define (list-diff u v)
  (map - u v))


(define (list-norm1 v)
  (apply max (map abs v)))


(define (list-dist u v)
  (list-norm1 (list-diff u v)))


(define (matrix-diff m n)
  (map list-diff m n))


(define (matrix-norm1 m)
  (apply max (map list-norm1 m)))


(define (matrix-dist m n)
  (matrix-norm1 (matrix-diff m n)))


(define basic-linalg-tests
  (test-suite
   "Tests basic-linalg"
   (test-case
    "test dot product"
    (let ([v (list 2 -2.4 4.8 -1.1)]
          [w (list 3.6 0.5 -1.9 -1.7)]
          [expected -1.25])
      (check-within (dot-product v w) expected 1e-6)))
   
   (test-case
    "test orthogonal vectors"
    (let ([v (list  1.0 0.0 -1.0 0.0)]
          [w (list -1.0 1.0 -1.0 2.0)]
          [expected 0.0])
      (check-within (dot-product v w) expected 1e-6)))
   
   (test-case
    "test parallel vectors"
    (let ([u (list  0.42 0.72 0.5 0.23)]
          [expected 0.9977])
      (check-within (dot-product u u) expected 1e-6)))
   
   (test-case
    "test matrix-vector multiplication"
    (let ([A (list
              (list  1  4  0)
              (list  4  2  0)
              (list  1  5 -2)
              (list -1  4 -4))]
          [v (list -3 3 4)]
          [expected (list 9 -6 4 -1)])
      (check-within (list-dist (matrix-*-vector A v) expected) 0 1e-6)))

   (test-case
    "test matrix transpose"
    (let ([A (list
              (list  1 12 10  3)
              (list  5  7  9  8)
              (list  6 11  2  4))]
          [expected (list
                     (list  1  5  6)
                     (list 12  7 11)
                     (list 10  9  2)
                     (list  3  8  4))])
      (check-within (matrix-dist (transpose A) expected) 0 1e-6)))
   
   (test-case
    "test matrix-matrix multiplication. https://en.wikipedia.org/wiki/Matrix_multiplication"
    (let ([A (list
              (list  1  0  1)
              (list  2  1  1)
              (list  0  1  1)
              (list  1  1  2))]
          [B (list
              (list 1 2 1)
              (list 2 3 1)
              (list 4 2 2))]
          [expected (list
                     (list  5 4 3)
                     (list  8 9 5)
                     (list  6 5 3)
                     (list 11 9 6))])
      (check-within (matrix-dist (matrix-*-matrix A B) expected) 0 1e-6)))))


(run-tests basic-linalg-tests)