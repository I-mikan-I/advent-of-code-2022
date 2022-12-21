#lang racket
(require "../utils/text.rkt")
(define input (file->string "day21/input.txt"))

(define-struct monkey (left right op)
  #:transparent)

(define (parse input)
  (hash-copy (for/hash ([m (string-split input "\n")])
               (match m
                 [(regexp #rx"(.*): ([0-9]*)$" (list _ name num)) (values name (string->number num))]
                 [(regexp #rx"(.*): (.*) \\+ (.*)" (list _ name left right))
                  (values name (monkey left right +))]
                 [(regexp #rx"(.*): (.*) \\* (.*)" (list _ name left right))
                  (values name (monkey left right *))]

                 [(regexp #rx"(.*): (.*) / (.*)" (list _ name left right))
                  (values name (monkey left right quotient))]
                 [(regexp #rx"(.*): (.*) - (.*)" (list _ name left right))
                  (values name (monkey left right -))]))))
(define h (parse input))
(define root (hash-ref h "root"))

(define (calculate monkeyinst)
  (cond
    [(number? monkeyinst) monkeyinst]
    [else
     (match-let*
         ([(monkey left right op) monkeyinst] [lmonk (hash-ref h left)] [rmonk (hash-ref h right)])
       (op (calculate lmonk) (calculate rmonk)))]))

(calculate root)

(hash-set! h "root" (monkey (monkey-left root) (monkey-right root) equal?))
(set! root (hash-ref h "root"))

(define (ex2)
  (match-define (monkey left right op) root)
  (define leftm (hash-ref h left))
  (define rightm (hash-ref h right))
  (hash-set! h "humn" 0)
  (define target (calculate rightm))
  (define upper
    (let rec ([i 2])
      (begin
        (hash-set! h "humn" i)
        (if (< (calculate leftm) target) i (rec (* i 2))))))
  (let rec ([lower 0] [upper upper] [middle (quotient upper 2)])
    (hash-set! h "humn" middle)
    (define result (calculate leftm))
    (cond
      [(> result target) (rec middle upper (+ middle (quotient (- upper middle) 2)))]
      [(< result target) (rec lower middle (+ lower (quotient (- middle lower) 2)))]
      [(equal? result target) middle])))

(ex2)
