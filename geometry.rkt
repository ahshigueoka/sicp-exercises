#lang racket
(define (average x y)
  (/ (+ x y) 2))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (scale-point scale point)
  (make-point
   (* scale (x-point point))
   (* scale (y-point point))))

(define (midpoint segment)
  (let ([start (start-segment segment)]
        [end (end-segment segment)])
    (make-point
     (average (x-point start) (x-point end))
     (average (y-point start) (y-point end)))))

(define (point-diff point1 point2)
  (make-point
   (- (x-point point1) (x-point point2))
   (- (y-point point1) (y-point point2))))

(define (segment-length segment)
  (norm (point-diff (start-segment segment) (end-segment segment))))

(define (norm point)
  (sqrt (+ (sqr (x-point point)) (sqr (y-point point)))))

(define (normalize point)
  (scale-point (/ 1.0 (norm point)) point))

(define (get-ortonormal vec)
  (normalize (make-point (- (y-point vec)) (x-point vec))))

(define (dot-product point1 point2)
  (+
   (* (x-point point1) (x-point point2))
   (* (y-point point1) (y-point point2))))

(define (projection from onto)
  (scale-point (dot-product from onto) onto))

; To define this rectangle, the segment
; (point1, point2) is the base. To find the other
; points, move the segment by `height` distance
; in the perpendicular direction
(define (rectangle-2pt-h point1 point2 height)
  (cons (make-segment point1 point2) height))

(rectangle-3pt A B C)
(define (rectangle-3pt point1 point2 anchor)
  (begin
    (define ortovec (get-ortonormal (point-diff point2 point1)))
    (define height (dot-product ortovec (point-diff anchor point1)))
    (cons (make-segment point1 point2) height)))

;(define A (make-point 1.0 2.0))
;(define B (make-point 4.0 6.0))
;(define C (make-point -3.8 5.6))
;(define D (make-point -4.4 4.8))
;(rectangle-2po-h A B 6.0)
;(rectangle-3pt A B C)
;(rectangle-3pt A B D)

(define (perimeter rectangle)
  (let ([width (segment-length (car rectangle))]
        [height (cdr rectangle)])
    (* 2 (+ width height))))

(define (area rectangle)
  (let ([width (segment-length (car rectangle))]
        [height (cdr rectangle)])
    (* width height)))

(define (print-point point)
  (newline)
  (display "(")
  (display (x-point point))
  (display ", ")
  (display (y-point point))
  (display ")"))