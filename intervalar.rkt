#lang racket
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (contains-zero? interval)
  (cond [(positive? (lower-bound interval)) false]
        [(negative? (upper-bound interval)) false]
        [else true]))

; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Exercise 2.11
(define (get-interval-type interval)
  (cond [(< (upper-bound interval) 0) 0]
        [(< 0 (lower-bound interval)) 2]
        [else 1]))

(define (mul-interval-2 x y)
  (let ([x-type (get-interval-type x)]
        [y-type (get-interval-type y)]
        [a (lower-bound x)]
        [b (upper-bound x)]
        [c (lower-bound y)]
        [d (upper-bound y)])
    (cond [(and (= x-type 0) (= y-type 0)) (make-interval (* b d) (* a c))]
          [(and (= x-type 0) (= y-type 1)) (make-interval (* a d) (* a c))]
          [(and (= x-type 0) (= y-type 2)) (make-interval (* a d) (* b c))]
          [(and (= x-type 1) (= y-type 0)) (make-interval (* b c) (* a c))]
          [(and (= x-type 1) (= y-type 2)) (make-interval (* a d) (* b d))]
          [(and (= x-type 2) (= y-type 0)) (make-interval (* b c) (* a d))]
          [(and (= x-type 2) (= y-type 1)) (make-interval (* b c) (* b d))]
          [(and (= x-type 2) (= y-type 2)) (make-interval (* a c) (* b d))]
          [else (make-interval (min (* a d) (* b c)) (max (* a c) (* b d)))])))

; Exercise 2.10
(define (div-interval x y)
  (if (contains-zero? y)
      (error "The dividing interval must not contain 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; interval-side interval
; returns
;     0 if a < b < 0
;     1 if a <= 0 <= b
;     2 if 0 < a < b
(define (interval-side interval)
  (cond [(< (upper-bound interval) 0) 0]
        [(< 0 (lower-bound interval)) 2]
        [else 1]))

(define (make-interval a b)
  (if (< b a)
      (error "The left number must be less than or equal to the right number")
      (cons a b)))

; Exercise 2.7
(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

; Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-center-width c (* (abs c) (/ p 100.0))))

(define (percent interval)
  (* 100.0 (/ (width interval) (center interval))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Exercise 2.13
(define (mul-interval-small interval-a interval-b)
  (let ([val-a (center interval-a)]
        [val-b (center interval-b)]
        [tol-a (width interval-a)]
        [tol-b (width interval-b)])
    (make-center-width (* val-a val-b) (+ (abs (* val-a tol-b)) (abs (* val-b tol-a))))))

; Exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; (define ResA (make-center-percent 100 0.5))
; (define ResB (make-center-percent 150 0.8))
; (par1 ResA ResB)
; (par2 ResA ResB)
; (center (par1 ResA ResB))
; (percent (par1 ResA ResB))
; (center (par2 ResA ResB))
; (percent (par2 ResA ResB))

