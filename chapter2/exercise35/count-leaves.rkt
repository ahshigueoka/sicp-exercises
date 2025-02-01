#lang racket

(define (count-leaves tree)
  (foldl (lambda (subtree count)
           (+ count
              (cond [(list? subtree) (count-leaves subtree)]
                    [(pair? subtree) (count-leaves subtree)]
                    [else 1])))
         0
         tree))

(provide count-leaves)