#lang racket
(require algorithms)

(define input-list (file->lines "input"))
(define replacements
  '(("one" "1") ("two" "2")
                ("three" "3")
                ("four" "4")
                ("five" "5")
                ("six" "6")
                ("seven" "7")
                ("eight" "8")
                ("nine" "9")))

(define first-list
  (map (λ (line) (car (regexp-match #rx"[0-9]|one|two|three|four|five|six|seven|eight|nine" line)))
       input-list))
(define last-list
  (map (λ (line)
         (list->string
          (reverse (string->list
                    (car (regexp-match #rx"[0-9]|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin"
                                       (list->string (reverse (string->list line)))))))))
       input-list))

(define first-fixed (map (λ (line) (string->number (regexp-replaces line replacements))) first-list))
(define last-fixed (map (λ (line) (string->number (regexp-replaces line replacements))) last-list))
(define zipped (zip first-fixed last-fixed))

(define pairs (map (λ (line) (+ (* 10 (first line)) (last line))) zipped))

(define answer (foldl + 0 pairs))

(displayln answer)
