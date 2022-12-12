#lang racket
(require "../utils/text.rkt")

(define strategy (hash "A" "X" "B" "Y" "C" "Z"))
(define wins (hash "X" "Z" "Y" "X" "Z" "Y"))
(define scores (hash "X" 1 "Y" 2 "Z" 3))

(define (ex01 str)
  (match (string-split str "\n")
    [(cons head t)
     (define (rec head tail)
       (match (string-split head " ")
         [(list key expected)
          (define opp (hash-ref strategy key))
          (+ (+ (cond
                  [(opp . equal? . expected) 3]
                  [((hash-ref wins expected) . equal? . opp) 6]
                  [else 0])
                (hash-ref scores expected))
             (if (not (empty? tail)) (rec (car tail) (cdr tail)) 0))]
         [_ 0]))
     (rec head t)]))

(define win-returns (hash "X" 0 "Y" 3 "Z" 6))
(define ring '("X" "Y" "Z"))
(define (calc-return left right)
  (define idx (index-of ring left))
  (define idx2
    (modulo (+ idx
               (match right
                 ["X" -1]
                 ["Y" 0]
                 ["Z" 1]))
            3))
  ((hash-ref scores (list-ref ring idx2)) . + . (hash-ref win-returns right)))

(define (ex02 str)
  (match (string-split str "\n")
    [(cons head t)
     (define (rec head tail)
       (match (string-split head " ")
         [(list key expected)
          (define opp (hash-ref strategy key))
          ((calc-return opp expected) . + . (if (not (empty? tail)) (rec (car tail) (cdr tail)) 0))]
         [_ 0]))
     (rec head t)]))

(writeln (ex01 (file->string "day02/day02-input.txt")))
(writeln (ex02 (file->string "day02/day02-input.txt")))