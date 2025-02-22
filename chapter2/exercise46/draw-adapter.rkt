#lang racket/base
(require racket/draw
         racket/gui
         "frame.rkt")

(define WIDTH 255)
(define HEIGHT 255)

(define GLOBALFRAME (make-frame
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 1.0)))

(define target (make-bitmap (+ 1 WIDTH) (+ 1 HEIGHT)))
(define dc (new bitmap-dc% [bitmap target]))

(define (ratio-to-pixels vec)
  (make-vect (* (xcor-vect vec) WIDTH)
             (* (ycor-vect vec) HEIGHT)))

(define (bitmap-draw-line start end)
  (let ([v1 (ratio-to-pixels start)]
        [v2 (ratio-to-pixels end)])
    (send dc draw-line
          (xcor-vect v1) (ycor-vect v1)
          (xcor-vect v2) (ycor-vect v2))))

(define bitmap-new
  (begin
    (set! target (make-bitmap (+ 1 WIDTH) (+ 1 HEIGHT)))
    (set! dc (new bitmap-dc% [bitmap target]))))

(define bitmap-show (make-object image-snip% target))

(define bitmap-clear (send dc erase))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (bitmap-draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define contour (list (make-segment (make-vect 0 0) (make-vect 1 0))
                      (make-segment (make-vect 1 0) (make-vect 1 1))
                      (make-segment (make-vect 1 1) (make-vect 0 1))
                      (make-segment (make-vect 0 1) (make-vect 0 0))))

(define cross (list (make-segment (make-vect 0 0) (make-vect 1 1))
                    (make-segment (make-vect 1 0) (make-vect 0 1))))

(define diamond (list
                 (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
                 (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
                 (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
                 (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))))

; Too lazy to make the wave painter manually
(define letter-f (list
                 (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
                 (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
                 (make-segment (make-vect 1.0 1.0) (make-vect 0.8 0.8))
                 (make-segment (make-vect 0.8 0.8) (make-vect 0.2 0.8))
                 (make-segment (make-vect 0.2 0.8) (make-vect 0.2 0.6))
                 (make-segment (make-vect 0.2 0.6) (make-vect 0.6 0.6))
                 (make-segment (make-vect 0.6 0.6) (make-vect 0.6 0.4))
                 (make-segment (make-vect 0.6 0.4) (make-vect 0.2 0.4))
                 (make-segment (make-vect 0.2 0.4) (make-vect 0.2 0.0))
                 (make-segment (make-vect 0.2 0.0) (make-vect 0.0 0.0))))

(define contour-painter (segments->painter contour))
(define cross-painter (segments->painter cross))
(define diamond-painter (segments->painter diamond))
(define f-painter (segments->painter letter-f))

(define global-frame (make-frame (make-vect 0 0)
                                 (make-vect 1 0)
                                 (make-vect 0 1)))

(define sub-frame (make-frame (make-vect 0.1 0.2)
                              (make-vect 0.433 0.25)
                              (make-vect -0.25 0.433)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ([paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0))]
          [paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))])
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; Exercise 2.51
(define (below painter1 painter2)
  (let ([paint-up
         (transform-painter painter1
                            (make-vect 0.0 0.5)
                            (make-vect 1.0 0.5)
                            (make-vect 0.0 1.0))]
        [paint-down
         (transform-painter painter2
                            (make-vect 0.0 0.0)
                            (make-vect 1.0 0.0)
                            (make-vect 0.0 0.5))])
    (lambda (frame)
      (paint-up frame)
      (paint-down frame))))

(define (below2 painter1 painter2)
  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))

;(contour-painter global-frame)
;(contour-painter sub-frame)
;(cross-painter global-frame)
;(diamond-painter global-frame)

;(f-painter (make-frame (make-vect 0.0 0.0)
;                                        (make-vect 1.0 0.0)
;                                        (make-vect 0.0 1.0)))

;(f-painter (make-frame (make-vect 0.0 0.0)
;                       (make-vect 1.0 0.0)
;                       (make-vect 0.0 1.0)))
;((below2 f-painter diamond-painter) (make-frame
;                                    (make-vect 0.0 0.0)
;                                    (make-vect 1.0 0.0)
;                                    (make-vect 0.0 1.0)))

; Exercise 2.52a
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (right-split painter (- n 1))])
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (up-split painter (- n 1))])
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ([up (up-split painter (- n 1))]
            [right (right-split painter (- n 1))])
        (let ([top-left (beside up up)]
              [bottom-right (below right right)]
              [corner (corner-split painter (- n 1))])
          (beside (below painter up)
                  (below right corner))))))

(define (square-limit painter n)
  (let ([quarter (corner-split painter n)])
    (let ([half (beside quarter (flip-horiz quarter))])
      (below (flip-vert half) half))))

((square-limit f-painter 2) GLOBALFRAME)
bitmap-show