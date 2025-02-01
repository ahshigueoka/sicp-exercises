#lang sicp
(#%require sicp-pict)

;; paint-hires / paint-hi-res renamed to just paint
;(paint (below (beside diagonal-shading
;                      (rotate90 diagonal-shading))
;              (beside (rotate270 diagonal-shading)
;                      (rotate180 diagonal-shading))))

(define (flipped-pairs painter)
  (let ([painter2 (beside painter (flip-vert painter))])
    (below painter2 painter2)))

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

(define (split first second)
  (define (split-iter painter n)
    (if (= n 0)
        painter
        (let ([smaller (split-iter painter (- n 1))])
          (first painter (second smaller smaller)))))
  split-iter)

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ([up (up-split painter (- n 1))]
            [right (right-split painter (- n 1))])
        (let ([top-left (beside up up)]
              [bottom-right (below right right)]
              [corner (corner-split painter (- n 1))])
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ([quarter (corner-split painter n)])
    (let ([half (beside (flip-horiz quarter) quarter)])
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ([top (beside (tl painter) (tr painter))]
          [bottom (beside (bl painter) (br painter))])
      (below bottom top))))