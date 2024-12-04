#lang racket

(define input (string-split (string-trim (file->string "input") "\n") ","))

(define (HASH str)
  (for/fold ([accum 0]) ([c (string->list str)])
    (modulo (* 17 (+ (char->integer c) accum)) 256)))

(define answer (foldl + 0 (map HASH input)))

(println answer)
