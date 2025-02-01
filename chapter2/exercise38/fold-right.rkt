#lang racket

(print "(foldr / 1 (list 1 2 3 4)): ")
(foldr / 1 (list 1 2 3 4))
(newline)

(print "(foldl / 1 (list 1 2 3 4)): ")
(foldl / 1 (list 1 2 3 4))
(newline)

(print "(foldr + 1 (list 1 2 3 4)): ")
(foldr + 1 (list 1 2 3 4))
(newline)

(print "(foldl + 1 (list 1 2 3 4)): ")
(foldl + 1 (list 1 2 3 4))
(newline)

(print "(foldr list null (list 1 2 3)): ")
(foldr list null (list 1 2 3 4))
(newline)

(print "(foldl list null (list 1 2 3)): ")
(foldl list null (list 1 2 3 4))
(newline)

; (foldl op init seq) and (foldr op init seq)
; return the same result when op is commutative