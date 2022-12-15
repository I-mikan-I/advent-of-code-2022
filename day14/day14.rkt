#lang racket
(require racket/hash)
(require "../utils/text.rkt")
(define input (file->string "day14/input.txt"))

(define (read-line str)
  (define lst (string-split str " -> "))
  (let rec ([curr (first lst)] [rest (cdr lst)] [result (hash)])
    (if (empty? rest)
        result
        (begin
          (let-values ([(current next) (values (map string->number (string-split curr ","))
                                               (map string->number (string-split (first rest) ",")))])
            (define xf (min (first current) (first next)))
            (define xt (max (first current) (first next)))
            (define yf (min (second current) (second next)))
            (define yt (max (second current) (second next)))
            (rec (first rest)
                 (cdr rest)
                 (hash-union
                  result
                  (for*/hash ([i (in-inclusive-range xf xt)] [j (in-inclusive-range yf yt)])
                    (values (cons i j) #t))
                  #:combine (lambda (a _) a))))))))

(define (read-input str)
  (define lst (string-split str "\n"))
  (foldl (lambda (h1 h2) (hash-union h1 h2 #:combine (lambda (a b) a))) (hash) (map read-line lst)))

(define h (read-input input))
(define max-y (foldl max 0 (map cdr (hash-keys h))))

(define (fall-sand h)
  (let rec ([state h] [count 0])
    (define-values (x y _)
      (for/fold ([x 500] [y 0] [break #f]) ([_ 100000000])
        #:break break
        (cond
          [(equal? y max-y) (values x y #t)]
          [(not (hash-has-key? state (cons x (+ y 1)))) (values x (+ y 1) #f)]
          [(not (hash-has-key? state (cons (- x 1) (+ y 1)))) (values (- x 1) (+ y 1) #f)]
          [(not (hash-has-key? state (cons (+ x 1) (+ y 1)))) (values (+ x 1) (+ y 1) #f)]
          [else (values x y #t)])))
    (if (>= y max-y) count (rec (hash-set state (cons x y) #t) (+ count 1)))))

(define (fall-sand-2 h)
  (let rec ([state h] [count 0])
    (define-values (x y _)
      (for/fold ([x 500] [y 0] [break #f]) ([_ 100000000])
        #:break break
        (cond
          [(equal? y (+ max-y 1)) (values x y #t)]
          [(not (hash-has-key? state (cons x (+ y 1)))) (values x (+ y 1) #f)]
          [(not (hash-has-key? state (cons (- x 1) (+ y 1)))) (values (- x 1) (+ y 1) #f)]
          [(not (hash-has-key? state (cons (+ x 1) (+ y 1)))) (values (+ x 1) (+ y 1) #f)]
          [else (values x y #t)])))
    (if (equal? (cons x y) (cons 500 0)) count (rec (hash-set state (cons x y) #t) (+ count 1)))))

(fall-sand h)
(fall-sand-2 h)
