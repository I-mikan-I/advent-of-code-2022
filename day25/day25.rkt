#lang racket
(require "../utils/text.rkt")
(define input (file->string "day25/input.txt"))

(define (snafu->number str)
  (define nums
    (map (lambda (cha)
           (match cha
             [#\- -1]
             [#\= -2]
             [num (string->number (list->string (list num)))]))
         (string->list str)))
  (for/sum ([val (reverse nums)] [i (length nums)]) (* (expt 5 i) val)))

(define (number->snafu num)
  (let rec ([res '()] [quot num])
    (cond
      [(equal? quot 0) (list->string res)]
      [else
       (define-values (nquot rem) (quotient/remainder quot 5))
       (case rem
         [(0) (rec (cons #\0 res) nquot)]
         [(1) (rec (cons #\1 res) nquot)]
         [(2) (rec (cons #\2 res) nquot)]
         [(3) (rec (cons #\= res) (+ nquot 1))]
         [(4) (rec (cons #\- res) (+ nquot 1))])])))

(number->snafu (for/sum ([line (string-split input "\n")]) (snafu->number line)))
