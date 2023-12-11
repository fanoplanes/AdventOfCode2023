#lang racket

(define input (file->lines "input"))

(define split-input (map (lambda (line) (regexp-split #rx"\\:|\\|" line)) input))

(define hehe
  (map (lambda (line) (map (lambda (part) (regexp-match* #rx"[0-9]+" part)) line)) split-input))

(define setted (map (lambda (line) (map (lambda (part) (list->set part)) line)) hehe))

(define intersection (map (lambda (line) (set-intersect (second line) (third line))) setted))

(define number-of-matches (map (lambda (line) (set-count line)) intersection))

(define (funct n tickets)
  (cond
    [(= n 0) 0]
    [else
     (+ n
        (let loop ([n n] [tickets tickets] [sum 0])
          (cond
            [(= n 0) sum]
            [else
             (let ([recursion (funct (car tickets) (cdr tickets))])
               (loop (sub1 n) (cdr tickets) (+ sum recursion)))])))]))

(define answer (funct (length number-of-matches) number-of-matches))

(displayln answer)
