#lang racket

(require rebellion/base/range)

(define input (file->string "input"))

(define split-input (regexp-split #rx"\n\n" input))

(define fixed-input (map (λ (inputs) (regexp-replace #rx"\n$" inputs "")) split-input))

(define seeds (map (λ (num) (string->number num)) (regexp-match* #px"\\d+" (car fixed-input))))

(define maps (map (λ (one-of-them) (regexp-split #rx"\n" one-of-them)) (cdr fixed-input)))

(define mappings
  (map (λ (one-of-them) (cdr (map (λ (lines) (regexp-match* #px"\\d+" lines)) one-of-them))) maps))

(define mappings-numbers
  (map (λ (one-of-them)
         (map (λ (lines) (map (λ (element) (string->number element)) lines)) one-of-them))
       mappings))

(define ranges
  (map (λ (one-of-them)
         (map (λ (lines)
                (list (- (first lines) (second lines))
                      (range (inclusive-bound (second lines))
                             (exclusive-bound (+ (second lines) (third lines))))))
              one-of-them))
       mappings-numbers))

(define (stuff lst num)
  (or (for/or ([i lst])
        (cond
          [(range-contains? (second i) num) (+ num (first i))]
          [else #f]))
      num))

(define (the-funny num)
  (stuff (seventh ranges)
         (stuff (sixth ranges)
                (stuff (fifth ranges)
                       (stuff (fourth ranges)
                              (stuff (third ranges)
                                     (stuff (second ranges) (stuff (first ranges) num))))))))

(define answer (car (sort (map (lambda (num) (the-funny num)) seeds) <)))

(println answer)
