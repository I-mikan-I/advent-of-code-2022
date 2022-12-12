#lang racket

(require "../utils/text.rkt")

(define (ex01)
  (define lst (string->list (file->string "day06/input.txt")))
  (for/fold ([last (take lst 4)] [i 0] #:result (+ i 4)) ([c (drop lst 4)])
    (define lst2 (cdr (append last (list c))))
    #:break (not (check-duplicates last))
    (values lst2 (+ i 1))))

(define (ex02)
  (define lst (string->list (file->string "day06/input.txt")))
  (for/fold ([last (take lst 14)] [i 0] #:result (+ i 14)) ([c (drop lst 14)])
    (define lst2 (cdr (append last (list c))))
    #:break (not (check-duplicates last))
    (values lst2 (+ i 1))))

(ex01)
(ex02)