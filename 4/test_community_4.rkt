#lang racket

(require "community_4.rkt")
;; local helpers
(require "../../homeworks/4/hw4.rkt")
(require rackunit)

(define tests
  (test-suite
   "Tests for Community 4"

   ;; 1
   (check-equal? (palindromic (list 1 2 4 8)) (list 9 6 6 9) "palindromic-1")
   (check-equal? (palindromic (list 1 2 4)) (list 5 4 5) "palindromic-2")
   (check-equal? (palindromic (list)) (list) "palindromic-3")

   ;; 2
   (check-equal? (stream-for-n-steps fibonacci 10) (list 0 1 1 2 3 5 8 13 21 34) "fibonacci")

   ;; 3
   (check-equal? (let ([x 0])
                   (begin
                     (stream-until
                      (λ (y)
                        (if (> y 10)
                            #f
                            (set! x (+ x 1)))) fibonacci)
                     x)) 7 "stream-until")

   ;; 11
   (check-equal? (perform 1 if #t) 1 "perform-1")
   (check-equal? (perform 1 if #f) #f "perform-2")
   (check-equal? (perform 1 unless #t) #t "perform-3")
   (check-equal? (perform 1 unless #f) 1 "perform-4")

   ))

(require rackunit/text-ui)
(run-tests tests)