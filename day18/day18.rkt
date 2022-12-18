#lang racket
(require "../utils/text.rkt")
(define input (file->string "day18/input.txt"))

(define h (make-hash))

(define sides 0)
(define max-x 0)
(define max-y 0)
(define max-z 0)

(define (ex1 input)
  (define lst (string-split input "\n"))
  (define lst- (map (lambda (triple) (map string->number (string-split triple ","))) lst))
  (for ([triple lst-])
    (set! sides (+ sides 6))
    (hash-set! h triple #t)
    (for ([offset '((0 0 1) (0 0 -1) (0 1 0) (0 -1 0) (1 0 0) (-1 0 0))])
      (match-define (list x y z) triple)
      (set! max-x (max max-x (+ x 1)))
      (set! max-y (max max-y (+ y 1)))
      (set! max-z (max max-z (+ z 1)))
      (match-define (list xo yo zo) offset)
      (if (hash-has-key? h (list (+ x xo) (+ y yo) (+ z zo))) (set! sides (- sides 2)) (void)))))

(define (calculate-outside h)
  (define start (list max-x max-y max-y))
  (define outside (make-hash))
  (let rec ([current start])
    (hash-set! outside current #t)
    (match-define (list x y z) current)
    (if (or (> (abs x) max-x) (> (abs y) max-y) (> (abs z) max-z))
        (void)
        (for ([offset '((0 0 1) (0 0 -1) (0 1 0) (0 -1 0) (1 0 0) (-1 0 0))])
          (match-define (list xo yo zo) offset)
          (define next (list (+ x xo) (+ y yo) (+ z zo)))
          (if (or (hash-has-key? h next) (hash-has-key? outside next)) (void) (rec next)))))
  outside)

(ex1 input)
(define outside (calculate-outside h))

(define (calculate-inside h outside)
  (define start (list max-x max-y max-y))
  (define inside (make-hash))
  (for* ([x (in-inclusive-range 0 max-x)]
         [y (in-inclusive-range 0 max-y)]
         [z (in-inclusive-range 0 max-z)])
    (if (or (hash-has-key? outside (list x y z)) (hash-has-key? h (list x y z)))
        (void)
        (hash-set! inside (list x y z) #t)))
  inside)

(define inside (calculate-inside h outside))
(writeln sides)

(writeln (- sides
            (for/sum ([triple (hash-keys h)])
                     (for/sum ([offset '((0 0 1) (0 0 -1) (0 1 0) (0 -1 0) (1 0 0) (-1 0 0))])
                              (match-define (list x y z) triple)
                              (match-define (list xo yo zo) offset)
                              (define next (list (+ x xo) (+ y yo) (+ z zo)))
                              (if (hash-has-key? inside next) 1 0)))))
