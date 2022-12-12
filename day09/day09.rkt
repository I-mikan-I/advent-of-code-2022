#lang racket

(require "../utils/text.rkt")
(define input (file->string "day09/input.txt"))

(define h (make-hash))
(define-struct tail (position-abs)
  #:transparent)
(define (calculate-move head tail-)
  (define diff--
    (cons (- (car (tail-position-abs head)) (car (tail-position-abs tail-)))
          (- (cdr (tail-position-abs head)) (cdr (tail-position-abs tail-)))))
  (define diff (cons (abs (car diff--)) (abs (cdr diff--))))
  (define-values (x y)
    (match diff
      [(cons 2 2) (values 1 1)]
      [(cons 2 n) (values 1 n)]
      [(cons n 2) (values n 1)]
      [(cons x y) (values 0 0)]))
  (define sign-adjusted (cons (if (< (car diff--) 0) (- x) x) (if (< (cdr diff--) 0) (- y) y)))
  (let ([abs (tail-position-abs tail-)])
    (struct-copy tail
                 tail-
                 (position-abs (cons (+ (car abs) (car sign-adjusted))
                                     (+ (cdr abs) (cdr sign-adjusted)))))))

(define (solve str tail-num)
  (define lst (string-split str "\n"))
  (define offsets
    (append*
     '()
     (map (lambda (current)
            (begin
              (define-values (offset n)
                (match (string->list current)
                  [(list #\L #\  num ...) (values '(-1 . 0) (string->number (list->string num)))]
                  [(list #\R #\  num ...) (values '(1 . 0) (string->number (list->string num)))]
                  [(list #\U #\  num ...) (values '(0 . 1) (string->number (list->string num)))]
                  [(list #\D #\  num ...) (values '(0 . -1) (string->number (list->string num)))]))
              (define unwrapped
                (for/list ([i n])
                  offset))
              unwrapped))
          lst)))
  (define head-positions
    (reverse
     (for/fold ([result (list (tail (cons 1000000 1000000)))]) ([offset offsets])
       (define current (car result))
       (define-values (x y) (let ([abs (tail-position-abs current)]) (values (car abs) (cdr abs))))
       (define next
         (struct-copy tail current (position-abs (cons (+ x (car offset)) (+ y (cdr offset))))))
       (append (list next) result))))
  (define result
    (for/fold ([head head-positions]) ([i tail-num])
      (define result
        (reverse (for/fold ([positions (list (tail (cons 1000000 1000000)))]) ([head- head])
                   (define current (car positions))
                   (define next (calculate-move head- current))
                   (cons next positions))))
      result))
  (for ([move result])
    (hash-set! h (tail-position-abs move) #t))
  result)

(define _ (solve input 1))
(writeln (hash-count h))
(set! h (make-hash))
(define _- (solve input 9))
(writeln (hash-count h))
