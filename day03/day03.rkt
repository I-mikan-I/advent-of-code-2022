#lang racket
(require "../utils/text.rkt")
;(define prios (hash #\p 16 #\L 38 #\P 42 #\v 22 #\t 20 #\s 19))

(define (prios c)
  (if (char-lower-case? c)
      (+ (- (char->integer c) (char->integer #\a)) 1)
      (+ (- (char->integer c) (char->integer #\A)) 27)))

(define (get-prio str)
  (define slst (string->list str))
  (define-values (front back) (split-at slst (/ (length slst) 2)))
  (let rec ([rest front])
    (if (list? (member (car rest) back)) (prios (car rest)) (rec (cdr rest)))))

(define (ex01 s)
  (define lst (map get-prio (string-split s "\n")))
  (foldl (lambda (a b) (+ a b)) 0 lst))

(define (get-prio-2 s1 s2 s3)
  (let rec ([rest s1])
    (if (and (list? (member (car rest) s2)) (list? (member (car rest) s3)))
        (prios (car rest))
        (rec (cdr rest)))))

(define (ex02 s)
  (define lst (map string->list (string-split s "\n")))
  (let rec ([lst lst])
    (match lst
      ['() 0]
      [(list s1 s2 s3 rest ...) (+ (get-prio-2 s1 s2 s3) (rec rest))])))
