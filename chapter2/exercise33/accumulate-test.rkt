#lang racket
(require rackunit "accumulate.rkt")
(require rackunit/text-ui)

(define accumulate-tests
  (test-suite
   "Test implementations of common functions as folding"
   (test-case
    "Implement map as folding"
    (let ([seq_a (list 1 3 4 6)]
          [seq_b (list 5 7 8 9 0)]
          [seq_c null])
      (check-equal? (custom-map (lambda (x) (* 2 x)) seq_a)
                    (map (lambda (x) (* 2 x)) seq_a))))
   (test-case
    "Implement append as folding"
    (let ([seq_a (list 1 3 4 6)]
          [seq_b (list 5 7 8 9 0)]
          [seq_c null])
      (check-equal? (custom-append seq_a seq_b)
                    (append seq_a seq_b))))
   (test-case
    "Implement length as folding"
    (let ([seq_a (list 1 3 4 6)]
          [seq_b (list 5 7 8 9 0)]
          [seq_c null])
      (check-equal? (custom-length seq_a)
                    (length seq_a))
      (check-equal? (custom-length seq_c)
                    (length seq_c))))))

(run-tests accumulate-tests)