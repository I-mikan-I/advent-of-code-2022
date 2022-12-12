#lang racket
(require "../utils/text.rkt")

(define (extract-sectors str)
  (map (lambda (lst) (map string->number lst))
       (match (string->list str)
         [(list n1 ... #\- n2 ... #\, n3 ... #\- n4 ...)
          (list (list (list->string n1) (list->string n2))
                (list (list->string n3) (list->string n4)))])))

(define (ex-01 s)
  (define lst (string-split s "\n"))
  (let* ([lst (map extract-sectors lst)]
         [lst (map (lambda (sectors)
                     (cond
                       [(and (<= (first (first sectors)) (first (second sectors)))
                             (>= (second (first sectors)) (second (second sectors))))
                        1]
                       [(and (>= (first (first sectors)) (first (second sectors)))
                             (<= (second (first sectors)) (second (second sectors))))
                        1]
                       [else 0]))
                   lst)])
    (foldl + 0 lst)))

(define (ex-02 s)
  (define lst (string-split s "\n"))
  (let* ([lst (map extract-sectors lst)]
         [lst (map (lambda (sectors)
                     (cond
                       [(and (<= (first (first sectors)) (second (second sectors)))
                             (>= (first (first sectors)) (first (second sectors))))
                        1]
                       [(and (<= (first (second sectors)) (second (first sectors)))
                             (>= (first (second sectors)) (first (first sectors))))
                        1]
                       [else 0]))
                   lst)])
    (foldl + 0 lst)))

(define input01 (file->string "day04/input.txt"))
