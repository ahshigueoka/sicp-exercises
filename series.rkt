#lang racket

(define (tautology x) true)

(define (even? n) (= 0 (remainder n 2)))

(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (sum-series term a b next)
  (accumulate term a b 0 next +))

(define (sum-squares a b)
  (sum-series square a b inc))

(define (sum-cubes a b)
  (sum-series cube a b inc))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (* 8 (sum-series pi-term a b pi-next)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum-series f (+ a (/ dx 2.0)) b add-dx)
     dx))

(define (simpsons-rule f a b n)
  (define (coefficient k)
    (cond [(= k 0) 1]
          [(= k n) 1]
          [(even? k) 2]
          [else 4]))
  (define h (/ (- b a) n))
  (define (term k)
    (* (coefficient k) (f (+ a (* k h)))))
  (* (sum-series term 0 n inc) (/ h 3.0)))

(define (product term a b next)
    (accumulate term a b 1 next *))

(define (pi-parcel k)
  (define numerator
    (+ 2.0 (* 2 (quotient (+ k 1) 2))))
  (define denominator
    (+ 3.0 (* 2 (quotient k 2))))
  (/ numerator denominator))

(define (accumulate term a b start next operation)
  (filtered-accumulate term a b start next operation tautology))

(define (filtered-accumulate term a b start next operation filter)
  (define (iteration count partial)
    (cond [(> count b) partial]
          [(filter count) (iteration (next count) (operation partial (term count)))]
          [else (iteration (next count) partial)]))
  (iteration a start))