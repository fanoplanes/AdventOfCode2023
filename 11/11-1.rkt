#lang racket

(define input (map (lambda (line) (string->list line)) (file->lines "input")))

(define (access-elem lst i j)
  (list-ref (list-ref lst i) j))

(define (find-galaxies lst)
  (for*/list ([i (in-range (length lst))]
              [j (in-range (length (car lst)))]
              #:when (char=? #\# (access-elem lst i j)))
    (list i j)))

(define (row-empty? lst row)
  (for/and ([j (in-range (length (car lst)))])
    (char=? #\. (access-elem lst row j))))

(define (col-empty? lst col)
  (for/and ([i (in-range (length lst))])
    (char=? #\. (access-elem lst i col))))

(define (empty-rows-between lst i1 i2)
  (cond
    [(< i1 i2) (for*/sum ([di (in-range i1 i2)] #:when (row-empty? lst di)) 1)]
    [(< i2 i1) (for*/sum ([di (in-range i2 i1)] #:when (row-empty? lst di)) 1)]
    [else 0]))

(define (empty-cols-between lst j1 j2)
  (cond
    [(< j1 j2) (for*/sum ([dj (in-range j1 j2)] #:when (col-empty? lst dj)) 1)]
    [(< j2 j1) (for*/sum ([dj (in-range j2 j1)] #:when (col-empty? lst dj)) 1)]
    [else 0]))

(define stretch (sub1 2))

(define galaxies (find-galaxies input))

(for*/sum
 ([i (in-range (length galaxies))] [j (in-range i (length galaxies))])
 (+ (abs (- (car (list-ref galaxies i)) (car (list-ref galaxies j))))
    (abs (- (cadr (list-ref galaxies i)) (cadr (list-ref galaxies j))))
    (* stretch (empty-rows-between input (car (list-ref galaxies i)) (car (list-ref galaxies j))))
    (* stretch (empty-cols-between input (cadr (list-ref galaxies i)) (cadr (list-ref galaxies j))))))
