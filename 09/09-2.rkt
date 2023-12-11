#lang racket

(define input (map (lambda (line) (string-split line)) (file->lines "input")))

(define nums (map (lambda (line) (map (lambda (num) (string->number num)) line)) input))

(define (diffs lst)
  (for/list ([i (in-range 1 (length lst))])
    (- (list-ref lst i) (list-ref lst (sub1 i)))))

(define (all-zero? lst)
  (andmap (lambda (entry) (eq? entry 0)) lst))

(define (predict lst)
  (if (all-zero? lst) 0 (- (first lst) (predict (diffs lst)))))

(for/sum ([lst nums]) (predict lst))
