#lang racket

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (begin
       (newline)
       (display n)
       (report-prime (- (current-inexact-milliseconds) start-time))
       #t)
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start how-many)
  (cond [(= how-many 0) #t]
        [(timed-prime-test start)
         (search-for-primes (inc start) (dec how-many))]
        [else (search-for-primes (inc start) how-many)]))

(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else false]))

(define (mr-test n)
  (define (try-it a)
    (= (expmod-sqrt a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (nontrivial-sqrt? a n)
  (begin
    (define rest (remainder (square a) n))
    (cond [(= a 1) rest]
          [(= a (- n 1)) rest]
          [(= rest 1) 0]
          [else rest])))

(define (expmod-sqrt base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (nontrivial-sqrt? (expmod-sqrt base (/ exp 2) m) m)]
        [else
         (remainder (* base (expmod-sqrt base (- exp 1) m))
                    m)]))

(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m)]
        [else
         (remainder (* base (expmod base (- exp 1) m))
                    m)]))