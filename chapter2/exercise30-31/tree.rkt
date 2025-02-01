#lang racket
; Exercise 2.30

; Using map
(define (square-tree root)
  (map (lambda (sub-tree)
         (if (list? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       root))

; Without map
(define (square-tree-nomap root)
  (cond [(null? root) null]
        [(not (list? root)) (* root root)]
        [else (cons (square-tree-nomap (car root))
                    (square-tree-nomap (cdr root)))]))

; Exercise 2.31
(define (tree-map transform root)
  (map (lambda (sub-tree)
         (if (list? sub-tree)
             (tree-map transform sub-tree)
             (transform sub-tree)))
       root))

(provide square-tree
         square-tree-nomap
         tree-map)