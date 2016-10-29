#lang racket

(provide (all-defined-out))

;; 1
(define (palindromic lst)
  (letrec ([f (lambda (begin end)
                (cond [(> begin end) (list)]
                      [(= begin end) (list (* 2 (list-ref lst begin)))]
                      [#t (let ([sum-list (list (+ (list-ref lst begin) (list-ref lst end)))])
                            (append sum-list (f (+ begin 1) (- end 1)) sum-list))]))])
    (f 0 (- (length lst) 1))))