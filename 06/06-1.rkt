#lang racket

(define input (file->lines "input"))

(define split (map (lambda (line) (regexp-match* #px"\\d+" line)) input))
(define conn (map (lambda (line) (string-join line)) split))
(define nums (map (lambda (line) (regexp-replace* #px"\\s+" line "")) conn))
(define num (map (lambda (line) (string->number line)) nums))

(define answer-ish
  (list (inexact->exact
         (ceiling (+ 0.001
                     (* 0.5
                        (- (first num) (sqrt (- (* (first num) (first num)) (* 4 (second num)))))))))
        (inexact->exact
         (floor (- (* 0.5 (+ (first num) (sqrt (- (* (first num) (first num)) (* 4 (second num))))))
                   0.001)))))

(define x (+ 1 (- (second answer-ish) (first answer-ish))))

(println x)
