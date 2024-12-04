#lang racket

(define input-list (file->lines "input"))

(define row
  (map (λ (line)
         (list (car (regexp-match #rx"Game [0-9]+" line))
               (string-join (regexp-match* #rx"[0-9]+ red" line))
               (string-join (regexp-match* #rx"[0-9]+ green" line))
               (string-join (regexp-match* #rx"[0-9]+ blue" line))))
       input-list))

(define funny (map (λ (game) (map (λ (roundd) (regexp-match* #rx"[0-9]+" roundd)) game)) row))

(define elems
  (map (λ (game) (map (λ (roundd) (map (λ (elem) (string->number elem)) roundd)) game)) funny))

(define maxed (map (λ (game) (map (λ (roundd) (argmax max roundd)) game)) elems))

(define summed 0)

(void (map (λ (game) (set! summed (+ summed (* (cadr game) (caddr game) (cadddr game))))) maxed))

(println summed)
