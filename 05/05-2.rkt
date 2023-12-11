#lang racket

(require rebellion/base/range)

(define input (file->string "input"))

(define split-input (regexp-split #rx"\n\n" input))

(define fixed-input (map (lambda (inputs) (regexp-replace #rx"\n$" inputs "")) split-input))

(define seeds
  (map (lambda (num) (regexp-match* #px"\\d+" num))
       (regexp-match* #px"\\d+\\s+\\d+" (car fixed-input))))

(define num-seeds
  (map (lambda (couple)
         (range (inclusive-bound (string->number (first couple)))
                (exclusive-bound (+ (string->number (first couple))
                                    (string->number (second couple))))))
       seeds))

(define maps (map (lambda (one-of-them) (regexp-split #rx"\n" one-of-them)) (cdr fixed-input)))

(define mappings
  (map (lambda (one-of-them) (cdr (map (lambda (lines) (regexp-match* #px"\\d+" lines)) one-of-them)))
       maps))

(define mappings-numbers
  (map (lambda (one-of-them)
         (map (lambda (lines) (map (lambda (element) (string->number element)) lines)) one-of-them))
       mappings))

(define ranges
  (map (lambda (one-of-them)
         (map (lambda (lines)
                (list (- (first lines) (second lines))
                      (range (inclusive-bound (first lines))
                             (exclusive-bound (+ (first lines) (third lines))))))
              one-of-them))
       mappings-numbers))

(define (stuff lst num)
  (or (for/or ([i lst])
        (cond
          [(range-contains? (second i) num) (- num (first i))]
          [else #f]))
      num))

(define (the-funnier num)
  (stuff (first ranges)
         (stuff (second ranges)
                (stuff (third ranges)
                       (stuff (fourth ranges)
                              (stuff (fifth ranges)
                                     (stuff (sixth ranges) (stuff (seventh ranges) num))))))))

(define (seed-valid? num)
  (for/or ([i num-seeds])
    (range-contains? i num)))

;I lost patience with trying to do it cleverly, so I just bruteforced it
;I did a bit of bifurcation
(define answer
  (for/or ([i (in-range 27992400 27992600)])
    (cond
      [(seed-valid? (the-funnier i)) i]
      [else #f])))
(println answer)
