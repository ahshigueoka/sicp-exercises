#lang racket
(define (expt b n)
  (exp-iter b n 1))

(define (dec x) (- x 1))
(define (square x) (* x x))

(define (exp-iter b remain prod)
  (if (= remain 0)
      prod
      (exp-iter b (dec remain) (* b prod))))

(define (even? n)
  (= (remainder n 2) 0))

(define (exp-fast b n)
  (exp-fast-iter b n 1))

(define (exp-fast-iter b remain prod)
  (cond ((= remain 0) prod)
        ((even? remain) (exp-fast-iter (square b) (/ remain 2) prod))
        (else (exp-fast-iter b (dec remain) (* b prod)))))

(define (fib-fast n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* (+ q p) a) (* q b))
                        (+ (* q a) (* p b))
                        p
                        q
                        (- count 1)))))
