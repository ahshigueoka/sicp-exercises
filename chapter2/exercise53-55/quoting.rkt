#lang racket/base

; Exercise 2.53
(list 'a 'b 'c)

(list (list 'george))
(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))



; Exercise 2.54
(define (recursive-eq a b)
  (if (and (list? a) (list? b))
      (let ([a-is-null (null? a)]
            [b-is-null (null? b)])
        (cond [(and a-is-null b-is-null) #t]
              [(and a-is-null (not b-is-null)) #f]
              [(and (not a-is-null) b-is-null) #f]
              [else (and (recursive-eq (car a) (car b))
                         (recursive-eq (cdr a) (cdr b)))]))
      (eq? a b)))

; Exercise 2.55
; The expression ''abracadabra produces the same output as
; the command (quote (quote abracadabra))
; If using car, the output is the firt symbol, which is
; quote

(provide recursive-eq)