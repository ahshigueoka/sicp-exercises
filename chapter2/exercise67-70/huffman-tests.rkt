#lang racket/base
(require rackunit
         rackunit/text-ui
         "huffman.rkt")

; Exercise 2.67
(define decode-test
  (test-suite
   "Tests for decoding a message using Huffman code"
   (test-case
    "Decode a short message"
    (let ([sample-message (list 0 1 1 0 0 1 0 1 0 1 1 1 0)]
          [decoded-message '(A D A B B C A)]
          [sample-tree (make-code-tree (make-leaf 'A 4)
                                       (make-code-tree
                                        (make-leaf 'B 2)
                                        (make-code-tree (make-leaf 'D 1)
                                                        (make-leaf 'C 1))))])
      (check-equal? (decode sample-message sample-tree) decoded-message)))))

; Exercise 2.68
(define encode-test
  (test-suite
   "Tests for encoding a message using Huffman code"
   (test-case
    "Encode a short message"
    (let ([encoded-message (list 0 1 1 0 0 1 0 1 0 1 1 1 0)]
          [decoded-message '(A D A B B C A)]
          [sample-tree (make-code-tree (make-leaf 'A 4)
                                       (make-code-tree
                                        (make-leaf 'B 2)
                                        (make-code-tree (make-leaf 'D 1)
                                                        (make-leaf 'C 1))))])
      (check-equal? (encode decoded-message sample-tree) encoded-message)))))

; Exercise 2.69
(define generate-tree-test
  (test-suite
   "Test the creation of Huffman tree"
   (test-case
    "Create tree from an ordered list of frequencies"
    (let ([pairs '((A 4) (B 2) (C 1) (D 1))]
          [expected '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)])
      (check-equal? (generate-huffman-tree pairs) expected)))))

; Exercise 2.70
(define encoding-song-test
  (test-suite
   "Encode a song lyrics"
   (let ([pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))]
         [lyrics '(
                   GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                   SHA BOOM)])
     (printf "Huffman encoding takes ~a bits\n" (length (encode lyrics (generate-huffman-tree pairs))))
     (printf "Fixed-lengh encoding would take ~a bits\n" (* 3 (length lyrics))))))


(run-tests decode-test)
(run-tests encode-test)
(run-tests generate-tree-test)
(run-tests encoding-song-test)