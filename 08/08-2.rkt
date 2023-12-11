#lang racket

(define input (file->lines "input"))

(define steps (car input))

(define nodes (map (lambda (line) (regexp-match* #rx"[A-Z]+" line)) (cddr input)))

(define ht (make-hash))

(for ([ln nodes])
  (hash-set! ht (first ln) (rest ln)))

(define (step node str)
  (case str
    [(#\R) (second (hash-ref ht node))]
    [(#\L) (first (hash-ref ht node))]))

(define (solve start end)
  (for/fold ([current start] [counter 0] #:result counter) ([L/R (in-cycle steps)])
    #:break (string-suffix? current end)
    (values (step current L/R) (add1 counter))))

(define ghost
  (for/list ([start (in-list (hash-keys ht))] #:when (string-suffix? start "A"))
    (solve start "Z")))
(apply lcm ghost)
