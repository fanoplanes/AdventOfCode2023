#lang racket
(require algorithms)

(define input (file->lines "input"))

(define split (map (lambda (line) (regexp-match* #px"\\d+" line)) input))
(define numbers (map (lambda (line) (map (lambda (num) (string->number num)) line)) split))

(define zipped (zip (first numbers) (second numbers)))

(define answer-ish
  (map (lambda (one-pair)
         (list (inexact->exact (ceiling (+ 0.001
                                           (* 0.5
                                              (- (first one-pair)
                                                 (sqrt (- (* (first one-pair) (first one-pair))
                                                          (* 4 (second one-pair)))))))))
               (inexact->exact (floor (- (* 0.5
                                            (+ (first one-pair)
                                               (sqrt (- (* (first one-pair) (first one-pair))
                                                        (* 4 (second one-pair))))))
                                         0.001)))))
       zipped))

(define x (map (lambda (y) (+ 1 (- (second y) (first y)))) answer-ish))

(define answer (foldl * 1 x))

(println answer)
