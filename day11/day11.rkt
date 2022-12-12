#lang racket
(define-struct monkey (list operation test inspected)
  #:transparent
  #:mutable)

(define h
  (hash
   0
   (monkey '(85 79 63 72)
           (lambda (old) (* 17 old))
           (lambda (old) (if (equal? 0 (modulo old 2)) 2 6))
           0)
   1
   (monkey '(53 94 65 81 93 73 57 92)
           (lambda (old) (* old old))
           (lambda (old) (if (equal? 0 (modulo old 7)) 0 2))
           0)
   2
   (monkey '(62 63) (lambda (old) (+ old 7)) (lambda (old) (if (equal? 0 (modulo old 13)) 7 6)) 0)
   3
   (monkey '(57 92 56) (lambda (old) (+ old 4)) (lambda (old) (if (equal? 0 (modulo old 5)) 4 5)) 0)
   4
   (monkey '(67) (lambda (old) (+ old 5)) (lambda (old) (if (equal? 0 (modulo old 3)) 1 5)) 0)
   5
   (monkey '(85 56 66 72 57 99)
           (lambda (old) (+ old 6))
           (lambda (old) (if (equal? 0 (modulo old 19)) 1 0))
           0)
   6
   (monkey '(86 65 98 97 69)
           (lambda (old) (* 13 old))
           (lambda (old) (if (equal? 0 (modulo old 11)) 3 7))
           0)
   7
   (monkey '(87 68 92 66 91 50 68)
           (lambda (old) (+ old 2))
           (lambda (old) (if (equal? 0 (modulo old 17)) 4 3))
           0)))

(define hb
  (for/hash ([(k v) h])
    (values k (struct-copy monkey v))))

(define (round- ex2?)
  (for* ([i 8] [item (monkey-list (hash-ref h i))])
    (define m (hash-ref h i))
    (define new
      (if ex2? (modulo ((monkey-operation m) item) 9699690) (quotient ((monkey-operation m) item) 3)))
    (set-monkey-inspected! m (+ 1 (monkey-inspected m)))
    (set-monkey-list! m (cdr (monkey-list m)))
    (define next ((monkey-test m) new))
    (let* ([nm (hash-ref h next)] [nml (monkey-list nm)])
      (set-monkey-list! nm (append nml (list new))))))

(for ([i 20])
  (round- #f))

(define lst
  (sort (for/list ([m (hash-values h)])
          (monkey-inspected m))
        >))
(* (first lst) (second lst))

(set! h hb)
(for ([i 10000])
  (round- #t))

(define lst-
  (sort (for/list ([m (hash-values h)])
          (monkey-inspected m))
        >))
(* (first lst-) (second lst-))
