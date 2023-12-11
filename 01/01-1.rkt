#lang racket
(define input-list (file->lines "input"))
(define removed-letters
  (map (λ (line)
         (cons (string->number (car (regexp-match #rx"[0-9]" line)))
               (string->number (car (regexp-match #rx"[0-9]"
                                                  (list->string (reverse (string->list line))))))))
       input-list))
(define answer (foldl + 0 (map (λ (line) (+ (* 10 (car line)) (cdr line))) removed-letters)))
(displayln answer)
