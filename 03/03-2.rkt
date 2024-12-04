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
(define (star? chr)
  (eqv? chr #\*))

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

(define (next-to-a-star? index)
  (for/or ([c (adj-indexes index)])
    (cond
      [(star? (vector-ref table c)) c]
      [else #f])))

(define numbers
  (apply zip (list (regexp-match* #rx"[0-9]+" inputt) (regexp-match-positions* #rx"[0-9]+" inputt))))

(define starred
  (foldl append
         '()
         (map (lambda (num)
                (for/list ([nbr (in-range (caadr num) (cdadr num))])
                  (cons (next-to-a-star? nbr) (car num))))
              numbers)))

(define cleared (remove-duplicates (filter (lambda (pr) (number? (car pr))) starred)))

(define grouped (group-by car cleared))

(define yay
  (for/list ([a grouped] #:when (= (length a) 2))
    (list (cdr (first a)) (cdr (second a)))))

(define answer
  (foldl + 0 (map (lambda (uwu) (* (string->number (first uwu)) (string->number (second uwu)))) yay)))

(displayln answer)
