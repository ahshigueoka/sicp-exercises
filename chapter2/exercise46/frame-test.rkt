#lang racket/base
(require rackunit
         rackunit/text-ui
         "frame.rkt")

(define frame-utils-tests
  (test-suite
   "Frame utility functions"
   (test-case
    "Check vector constructor"
    (check-within (make-vect 0 0) (cons 0 0) 1e-6 "Null vector"))
   (test-case
    "Check x coordinate selector"
    (check-within (xcor-vect (make-vect 3 6)) 3 1e-6 "X coordinate"))
   (test-case
    "Check y coordinate selector"
    (check-within (ycor-vect (make-vect 8 6)) 6 1e-6 "Y coordinate"))
   (test-case
    "Check vector addition"
    (let ([vec1 (make-vect 3 4)]
          [vec2 (make-vect 2 -5)])
      (check-within (add-vect vec1 vec2) (make-vect 5 -1) 1e-6 "Vector sum")))
   (test-case
    "Check vector scaling"
    (let ([vec1 (make-vect 3 4)]
          [vec2 (make-vect 0 -1)])
      (check-within (scale-vect 1.5 vec1) (make-vect 4.5 6) 1e-6 "Vector scale")
      (check-within (scale-vect -2 vec2) (make-vect 0 2) 1e-6 "Vector scale")))
   (test-case
    "Check vector subtraction"
    (let ([vec1 (make-vect 3 4)]
          [vec2 (make-vect 2 -5)])
      (check-within (sub-vect vec1 vec2) (make-vect 1 9) 1e-6 "Vector sum")))))

(define frame-tests
  (test-suite
   "Test Frame class"
   (test-case
    "Check Frame constructor"
    (let* ([origin (make-vect 1 2)]
          [edge1 (make-vect 0.866 0.5)]
          [edge2 (make-vect -0.5 0.866)])
      (check-equal? (make-frame origin edge1 edge2) (list origin edge1 edge2) "Frame constructor")))
   (test-case
    "Check origin selector"
    (let* ([origin (make-vect 1 2)]
          [edge1 (make-vect 0.866 0.5)]
          [edge2 (make-vect -0.5 0.866)])
      (check-equal? (frame-origin (make-frame origin edge1 edge2)) origin "Origin selector")))
   (test-case
    "Check edge1 selector"
    (let* ([origin (make-vect 1 2)]
          [edge1 (make-vect 0.866 0.5)]
          [edge2 (make-vect -0.5 0.866)])
      (check-equal? (frame-edge1 (make-frame origin edge1 edge2)) edge1 "Origin selector")))
   (test-case
    "Check edge2 selector"
    (let* ([origin (make-vect 1 2)]
          [edge1 (make-vect 0.866 0.5)]
          [edge2 (make-vect -0.5 0.866)])
      (check-equal? (frame-edge2 (make-frame origin edge1 edge2)) edge2 "Origin selector")))))

(define frame-2-tests
  (test-suite
   "Test Frame class"
   (test-case
    "Check Frame constructor"
    (let* ([origin (make-vect 1 2)]
          [edge1 (make-vect 0.866 0.5)]
          [edge2 (make-vect -0.5 0.866)])
      (check-equal? (make-frame-2 origin edge1 edge2) (cons origin (cons edge1 edge2)) "Frame constructor")))
   (test-case
    "Check origin selector"
    (let* ([origin (make-vect 1 2)]
          [edge1 (make-vect 0.866 0.5)]
          [edge2 (make-vect -0.5 0.866)])
      (check-equal? (frame-origin (make-frame-2 origin edge1 edge2)) origin "Origin selector")))
   (test-case
    "Check edge1 selector"
    (let* ([origin (make-vect 1 2)]
          [edge1 (make-vect 0.866 0.5)]
          [edge2 (make-vect -0.5 0.866)])
      (check-equal? (frame-edge1 (make-frame-2 origin edge1 edge2)) edge1 "Origin selector")))
   (test-case
    "Check edge2 selector"
    (let* ([origin (make-vect 1 2)]
          [edge1 (make-vect 0.866 0.5)]
          [edge2 (make-vect -0.5 0.866)])
      (check-equal? (frame-2-edge2 (make-frame-2 origin edge1 edge2)) edge2 "Origin selector")))))


(define transform-tests
  (test-suite
   "Test frame transformation"
   (test-case
    "Move the vector from frame coordinate to global"
    (let* ([origin (make-vect 1 2)]
           [edge1 (make-vect 0.866 0.5)]
           [edge2 (make-vect -0.5 0.866)]
           [frame (make-frame origin edge1 edge2)])
      (check-within ((frame-coord-map frame) (make-vect 1.0 1.0))
                    (make-vect 1.366 3.366)
                    1e-6)))))


(define segment-tests
  (test-suite
   "Test Segment class"
   (test-case
    "Check Segment constructor"
    (let ([start (make-vect 0.5 0.5)]
          [end (make-vect 1 1)])
      (check-equal? (make-segment start end) (cons start end) "Segment constructor")))
   (test-case
    "Check start selector"
    (let* ([start (make-vect 0.5 0.5)]
           [end (make-vect 1 1)]
           [seg (make-segment start end)])
      (check-equal? (start-segment seg) start "Start selector")))
   (test-case
    "Check end selector"
    (let* ([start (make-vect 0.5 0.5)]
           [end (make-vect 1 1)]
           [seg (make-segment start end)])
      (check-equal? (end-segment seg) end "End selector")))))

(run-tests frame-utils-tests)
(run-tests frame-tests)
(run-tests frame-2-tests)
(run-tests segment-tests)
(run-tests transform-tests)