#lang racket

(define input (file->lines "input"))

(define split-input (map (lambda (line) (regexp-split #rx"\\:|\\|" line)) input))

(define hehe
  (map (lambda (line) (map (lambda (part) (regexp-match* #rx"[0-9]+" part)) line)) split-input))

(define setted (map (lambda (line) (map (lambda (part) (list->set part)) line)) hehe))

(define intersection (map (lambda (line) (set-intersect (second line) (third line))) setted))

(define number-of-matches (map (lambda (line) (set-count line)) intersection))

(define value
  (map (lambda (line)
         (cond
           [(eq? line 0) 0]
           [else (expt 2 (- line 1))]))
       number-of-matches))

(define answer (foldl + 0 value))

(displayln answer)
