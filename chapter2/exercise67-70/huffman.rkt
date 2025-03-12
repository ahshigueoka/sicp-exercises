#lang racket/base

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch
               (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        [else (error "bad bit -- CHOOSE-BRANCH" bit)]))

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set))) (cons x set)]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (find-symbol tree)
    (let ([left-sub (left-branch tree)]
          [right-sub (right-branch tree)])
         (cond [(and (leaf? left-sub) (eq? symbol (symbol-leaf left-sub))) (list 0)]
               [(and (leaf? right-sub) (eq? symbol (symbol-leaf right-sub))) (list 1)]
               [(member symbol (symbols left-sub)) (cons 0 (find-symbol left-sub))]
               [(member symbol (symbols right-sub)) (cons 1 (find-symbol right-sub))])))
  (if (member symbol (symbols tree))
      (find-symbol tree)
      '()))

(define (successive-merge ordered-set)
  (if (null? (cdr ordered-set))
      (car ordered-set)
      (successive-merge (adjoin-set (make-code-tree (car ordered-set)
                                                    (cadr ordered-set))
                                    (cddr ordered-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(provide make-leaf
         make-code-tree
         decode
         encode
         make-leaf-set
         generate-huffman-tree)