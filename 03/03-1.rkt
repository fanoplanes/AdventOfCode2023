#lang racket
(require algorithms)

(define input (file->string "input"))
(define inputt (regexp-replace* #rx"\n" input ""))

(define table
  (call-with-input-file "input"
                        (lambda (in)
                          (for*/vector ([line (in-lines in)] [char (in-string line)])
                            char))))

;(define s (sqrt (vector-length table)))
(define s 141)
(define (part-number? chr)
  (and (not (eqv? chr #\.)) (not (char-numeric? chr))))

(define (in-same-stride? ind1 ind2)
  (and (eq? (quotient ind1 s) (quotient ind2 s)) (eq? (sgn ind1) (sgn ind2))))

(define (index-filter index-list)
  (filter (lambda (ind) (and (>= ind 0) (< ind (vector-length table)))) index-list))

(define (ok? tested ind)
  (cond
    [(in-same-stride? tested ind) tested]
    [else -1]))

(define (adj-indexes ind)
  (index-filter (list (ok? (+ (- (- s) 1) ind) (- ind s))
                      (+ (- s) ind)
                      (ok? (+ (+ (- s) 1) ind) (- ind s))
                      (ok? (sub1 ind) ind)
                      (ok? (add1 ind) ind)
                      (ok? (+ (- s 1) ind) (+ s ind))
                      (+ s ind)
                      (ok? (+ (+ s 1) ind) (+ s ind)))))

(define (engine-part? index)
  (for/or ([c (adj-indexes index)])
    (part-number? (vector-ref table c))))

(define numbers
  (apply zip (list (regexp-match* #rx"[0-9]+" inputt) (regexp-match-positions* #rx"[0-9]+" inputt))))

(define included
  (map (lambda (x)
         (cond
           [(for/or ([indexx (in-range (caadr x) (cdadr x))])
              (engine-part? indexx))
            (car x)]
           [else #f]))
       numbers))

(define answer (foldl + 0 (map string->number (filter string? included))))

(displayln answer)
