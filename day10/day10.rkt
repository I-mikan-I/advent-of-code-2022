#lang racket
(require "../utils/text.rkt")

(define input (file->string "day10/input.txt"))

(define cycle 1)
(define x 1)
(define h (make-hash))

(define (noop)
  (hash-set! h cycle x)
  (set! cycle (+ cycle 1)))

(define (addx num)
  (hash-set! h (+ cycle 1) x)
  (hash-set! h cycle x)
  (set! cycle (+ cycle 2))
  (set! x (+ x num))
  (hash-set! h cycle x))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define input2 (string-append "(begin " (regexp-replace* #rx"(?m:^(.*)$)" input "(\\1)") ")"))
(define code (call-with-input-string input2 read))
(eval code ns)

(define sigs
  (for*/list ([cycle- '(20 60 100 140 180 220)])
    (* (hash-ref h cycle-) cycle-)))

(foldl + 0 sigs)

(for ([pixel (in-range 1 240)])
  (define inside (<= (abs (- (hash-ref h pixel) (modulo (- pixel 1) 40))) 1))
  (if inside (write "#") (write "."))
  (if (equal? 0 (modulo pixel 40)) (writeln "") (void)))
