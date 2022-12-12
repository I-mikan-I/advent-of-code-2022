#lang racket

(require "../utils/text.rkt")

(define input (file->string "day12/input.txt"))

(define (parse str)
  (define lines (map string->list (string-split str "\n")))
  (define lst
    (for/list ([line lines])
      (map (lambda (char)
             (match char
               [#\E 25]
               [#\S 0]
               [c (- (char->integer c) (char->integer #\a))]))
           line)))
  (define-values (end start)
    (for*/fold ([start '(0 . 0)] [end '(0. 0)] #:result (values end start))
               ([i (length lines)] [j (length (list-ref lines i))])
      (define start- (if (equal? (list-idx lines i j) #\S) (cons i j) start))
      (define end- (if (equal? (list-idx lines i j) #\E) (cons i j) end))
      (values start- end-)))
  (values lst end start))

(define-values (ll end start) (parse input))

(define (find lst end)
  (define (scan lst dist at)
    (define-values (x y) (values (car at) (cdr at)))
    (define current-dist (list-idx dist x y))
    (for/fold ([dist dist] [changed '()]) ([newat '((-1 . 0) (1 . 0) (0 . 1) (0 . -1))])
      (define-values (nx ny) (values (+ x (car newat)) (+ y (cdr newat))))
      (cond
        [(or (< nx 0) (< ny 0)) (values dist changed)]
        [(or (>= nx (length lst)) (>= ny (length (car lst)))) (values dist changed)]
        [(<= (- (list-idx lst nx ny) (list-idx lst x y)) -2) (values dist changed)]
        [else
         (define next-dist (list-idx dist nx ny))
         (if (> next-dist (+ current-dist 1))
             (values (list-idx-set dist nx ny (+ current-dist 1)) (cons (cons nx ny) changed))
             (values dist changed))])))
  (define dists2
    (map (lambda (row)
           (for*/list ([_ row])
             999999))
         lst))
  (define dists- (list-idx-set dists2 (car end) (cdr end) 0))

  (let rec ([work (list end)] [dists dists-])
    (match work
      ['() dists]
      [(list h rest ...)
       (define-values (new-dists changed) (scan lst dists h))
       (rec (append rest changed) new-dists)])))

(define result (find ll end))

(list-idx result (car start) (cdr start))

(first (sort (for*/list ([i (length ll)]
                         [j (length (list-ref ll i))]
                         #:when (equal? (list-ref (list-ref ll i) j) 0))
               (list-ref (list-ref result i) j))
             <))
