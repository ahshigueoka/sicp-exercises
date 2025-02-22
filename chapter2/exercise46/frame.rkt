#lang racket/base

; Exercise 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect scale vec)
  (make-vect (* scale (xcor-vect vec))
             (* scale (ycor-vect vec))))

(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect -1 v2)))

(define (diffmax-vect v1 v2)
  (max (abs (- (xcor-vect v1) (xcor-vect v2)))
       (abs (- (ycor-vect v1) (ycor-vect v2)))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin frame)
  (car frame))

(define (frame-edge1 frame)
  (cadr frame))

(define (frame-edge2 frame)
  (caddr frame))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; Exercise 2.47
; The only difference in functionality is in the
; method to extract the second edge
(define (frame-2-edge2 frame)
  (cddr frame))

; Exercise 2.48
(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (frame-coord-map frame)
  (lambda (vec)
    (add-vect
     (frame-origin frame)
     (add-vect (scale-vect (xcor-vect vec)
                           (frame-edge1 frame))
               (scale-vect (ycor-vect vec)
                           (frame-edge2 frame))))))

; Exercise 2.49


(provide make-vect
         xcor-vect
         ycor-vect
         add-vect
         scale-vect
         sub-vect
         diffmax-vect
         make-frame
         frame-origin
         frame-edge1
         frame-edge2
         make-frame-2
         frame-2-edge2
         make-segment
         start-segment
         end-segment
         frame-coord-map)