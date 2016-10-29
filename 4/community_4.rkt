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

;; 2
(define fibonacci
  (lambda () (cons 0
                   (lambda () (cons 1
                                    (letrec ([f (lambda (a b)
                                                  (cons (+ a b)
                                                        (lambda () (f b (+ a b)))))])
                                      (lambda () (f 0 1))))))))

;; 11
(define-syntax perform
  (syntax-rules (if unless)
    [(perform e1 if e2)
     (let ([r2 e2])
       (if r2
           e1
           r2))]
    [(perform e1 unless e2)
     ;; cannot just (perform e1 if (not e2)) because need to eval to e2 ife2 is #t
     (let ([r2 e2])
       (if (not r2)
           e1
           r2))]))