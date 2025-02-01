#lang racket
(require racket/math)

(define (inc n) (+ n 1))

(define (dec n) (- n 1))

(define (square x) (* x x))

(define (norm1 x y) (abs (- x y)))

(define (average a b)
  (/ (+ a b) 2))

(define (avg-damp func)
  (lambda (x) (average x (func x))))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond [(positive? test-value)
                 (search f neg-point midpoint)]
                [(negative? test-value)
                 (search f midpoint pos-point)]
                [else midpoint])))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (cont-frac n d k)
  (define (cont-frac-iter j partial)
    (if (<= j 0)
        partial
        (cont-frac-iter (dec j) (/ (n j) (+ (d j) partial)))))
  (cont-frac-iter k 0.0))


(define (euler-cf m)
  (define (denom k)
    (if (= 0 (remainder (+ k 1) 3))
        (* 2 (/ (+ k 1) 3))
        1))
  (cont-frac (lambda (j) 1.0) denom m))

(define (tan-cf x k)
  (define (numer j)
    (if (= j 1) x (- (* x x))))
  (cont-frac numer (lambda (j) (- (* 2 j) 1)) k))

(define (approx-deriv g dx)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g deriv)
  (lambda (x)
    (- x (/ (g x) (deriv x)))))

(define (newtons-method g deriv guess)
  (fixed-point (newton-transform g deriv) guess))

(define (cubic c2 c1 c0)
  (lambda (x)
  (+ c0 (* x (+ c1 (* x (+ c2 x)))))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (double func)
  (lambda (x) (func (func x))))

(define (repeated func times)
  (define (iteration chain count)
    (if (= 0 count)
        chain
        (iteration (compose func chain) (dec count))))
  (cond [(<= times 0) identity]
        [else (iteration func (dec times))]))

(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (smooth-n f dx n)
  (define (smooth-fix-dx func) (smooth func dx))
  ((repeated smooth-fix-dx n) f))

(define (fibo n)
  (define (iteration previous current count)
    (cond [(<= count 0) previous]
          [(= count 1) current]
          [else (iteration current (+ previous current) (dec count))]))
  (iteration 0 1 n))

(define (iterative-improve is-good-enough? improve-guess)
  (define (iterative-solve guess)
    (if (is-good-enough? guess)
        guess
        (iterative-solve (improve-guess guess))))
  (lambda (try-value) (iterative-solve try-value)))

(define (fixpoint-template func tol)
  (define (good-enough? guess) (> tol (norm1 guess (func guess))))
  (define (improve-guess guess) (average guess (func guess)))
  (iterative-improve good-enough? improve-guess))

(define (newton-template g tol)
  (define newton-f (newton-transform g (approx-deriv g 0.001)))
  (define (good-enough? guess) (> tol (norm1 guess (newton-f guess))))
  (define (improve-guess guess) (average guess (newton-f guess)))
  (iterative-improve good-enough? improve-guess))

(define (sqrt-template x x0)
  ((newton-template (lambda (y) (- (square y) x)) 0.000001) x0))