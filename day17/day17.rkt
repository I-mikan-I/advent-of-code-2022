#lang racket
(require "../utils/text.rkt")
(define input (file->string "day17/input.txt"))

(define-struct rock (fields height)
  #:transparent)

(define IH (rock '((0 . 0) (0 . 1) (0 . 2) (0 . 3)) 0))
(define PLUS (rock '((2 . 1) (1 . 0) (1 . 1) (1 . 2) (0 . 1)) 2))
(define L (rock '((2 . 2) (1 . 2) (0 . 0) (0 . 1) (0 . 2)) 2))
(define IV (rock '((3 . 0) (2 . 0) (1 . 0) (0 . 0)) 3))
(define SQ (rock '((1 . 0) (1 . 1) (0 . 0) (0 . 1)) 1))

(define height 0)

(define chamber
  (make-hash '(((0 . 1) . #t) ((0 . 2) . #t)
                              ((0 . 3) . #t)
                              ((0 . 4) . #t)
                              ((0 . 5) . #t)
                              ((0 . 6) . #t)
                              ((0 . 7) . #t)
                              ((0 . 8) . #t)))) ; rows

(define idx 0)
(define pattern (string->list input))

(define (get-row rok row)
  (for/fold ([res '()] [max 0] #:result res) ([pair (rock-fields rok)])
    (if (equal? row (car pair)) (values (append res (list pair)) max) (values res max))))

(define (check-legal rok row height movement)
  (for/and ([cord (rock-fields rok)])
    (define would-move (cons (+ (car cord) height (car movement)) (+ (cdr cord) row (cdr movement))))
    (if (hash-has-key? chamber would-move) #f #t)))

(define (fall rok)
  (for ([i (in-inclusive-range height (+ height (rock-height rok) 4))])
    (hash-set*! chamber (cons i 0) #t (cons i 8) #t))

  (define bottom (get-row rok 0))
  (define-values (h row)
    (let rec ([h (+ height 4)] [row 3])
      (define new-row
        (case (list-idx pattern idx)
          [(#\>) (if (check-legal rok row h '(0 . 1)) (+ row 1) row)]
          [(#\<) (if (check-legal rok row h '(0 . -1)) (- row 1) row)]))
      (define new-height (if (check-legal rok new-row h '(-1 . 0)) (- h 1) h))
      (set! idx (modulo (+ idx 1) (length pattern)))
      (if (equal? new-height h) (values new-height new-row) (rec new-height new-row))))
  (for ([c (rock-fields rok)])
    (hash-set! chamber (cons (+ (car c) h) (+ (cdr c) row)) #t))
  (set! height (max height (+ h (rock-height rok))))
  ; (printCh)
  (void))

(define cycle (list IH PLUS L IV SQ))

(define (printCh)
  (for ([i (in-range height 0 -1)])
    (for ([j 9])
      (if (hash-has-key? chamber (cons i j)) (printf "#") (printf ".")))
    (printf "\n")))

(define dyn (make-hash))
(define (find-cycle i)
  (define found-cycle
    (if (equal? 0 (modulo i (length cycle)))
        (if (and (hash-has-key? dyn idx) (> i 10000))
            (cons (car (hash-ref dyn idx))
                  (cons (- i (car (hash-ref dyn idx))) (- height (cdr (hash-ref dyn idx)))))
            (hash-set! dyn idx (cons i height)))
        (void)))
  (define rok (list-idx cycle (modulo i (length cycle))))
  (if (void? found-cycle)
      (begin
        (fall rok)
        (find-cycle (+ i 1)))
      found-cycle))

(define (cycle-offset offset)
  (match-define (cons start (cons cycle- height-)) (find-cycle 0))
  (define save height)
  (for ([i offset])
    (define dyn (make-hash))
    (define rok (list-idx cycle (modulo i (length cycle))))
    (if (equal? idx 0) (printf "cyclic at ~a\n" i) (void))
    (if (and (equal? 0 (modulo i (length cycle))) (equal? 257 idx)) (writeln height) (void))
    (fall rok))
  (- height save))

(define goal 1000000000000)
(match-define (cons start (cons cycle- height-)) (find-cycle 0))
(define to-add (* (quotient (- goal start) cycle-) height-))

(set! idx 0)
(set! chamber
      (make-hash '(((0 . 1) . #t) ((0 . 2) . #t)
                                  ((0 . 3) . #t)
                                  ((0 . 4) . #t)
                                  ((0 . 5) . #t)
                                  ((0 . 6) . #t)
                                  ((0 . 7) . #t)
                                  ((0 . 8) . #t))))
(set! height 0)

(for ([i start])
  (define dyn (make-hash))
  (define rok (list-idx cycle (modulo i (length cycle))))
  (if (equal? idx 0) (printf "cyclic at ~a\n" i) (void))
  (if (and (equal? 0 (modulo i (length cycle))) (equal? 257 idx)) (writeln height) (void))
  (fall rok))

(define offset-bottom height)
(set! idx 0)
(set! chamber
      (make-hash '(((0 . 1) . #t) ((0 . 2) . #t)
                                  ((0 . 3) . #t)
                                  ((0 . 4) . #t)
                                  ((0 . 5) . #t)
                                  ((0 . 6) . #t)
                                  ((0 . 7) . #t)
                                  ((0 . 8) . #t))))
(set! height 0)
(define offset-top (cycle-offset (remainder (- goal start) cycle-)))

(writeln height)
