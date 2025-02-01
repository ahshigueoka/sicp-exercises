#lang racket

(newline)
(print "Square of numbers 1 to 5")
(newline)
(print (map sqr (list 1 2 3 4 5)))

(newline)
(print "Odd numbers from 1 to 5")
(newline)
(print (filter odd? (list 1 2 3 4 5)))

(newline)
(print "Sum of numbers from 1 to 5")
(newline)
(print (foldl + 0 (range 1 6)))

(newline)
(print (range 2 8))

(newline)
(print "Iterate over tree")
(newline)
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(print (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))

(newline)
(print "Sum of odd squares iterating over tree")
(define (sum-odd-squares tree)
  (foldl +
         0
         (map sqr
              (filter odd?
                      (enumerate-tree tree)))))
(newline)
(print (sum-odd-squares (list 1 (list 2 (list 3 4)) 5)))

(newline)
(print "Square of Fibonacci numbers")
(define (fib n)
  (define (fib-iter k pprev prev)
    (if (> k n)
        prev
        (fib-iter (+ k 1) prev (+ pprev prev))))
  (cond [(< n 1) 0]
        [(= n 1) 1]
        [else (fib-iter 2 0 1)]))

(define (list-fib-squares n)
  (foldr cons
         null
         (map sqr
              (map fib
                   (range 0 (+ n 1))))))
(newline)
(print (list-fib-squares 10))

(define (product-of-squares-of-odd-elements sequence)
  (foldl *
         1
         (map sqr
              (filter odd? sequence))))
(newline)
(print "Product of squares of odd numbers from 1 to 5")
(newline)
(print (product-of-squares-of-odd-elements (range 1 6)))