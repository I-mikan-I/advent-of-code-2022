#lang racket
(require "../utils/text.rkt")
(define input (file->string "day08/input.txt"))

(define map2
  (map (lambda (lst) (map (lambda (char) (string->number (make-string 1 char))) (string->list lst)))
       (string-split input "\n")))

(define size (length map2))

(define inner
  (for*/fold ([sum 0]) ([i (in-range 1 (- size 1))] [k (in-range 1 (- size 1))])
    (let ([k- k] [i- i])
      (define highest (foldl max 0 (take (list-ref map2 i) k)))
      (define highest2 (foldl max 0 (drop (list-ref map2 i) (+ k 1))))
      (define lst
        (for/list ([j i])
          (list-ref (list-ref map2 j) k)))
      (define highest3 (foldl max 0 lst))
      (define lst-
        (for/list ([j (in-range (+ i 1) size)])
          (list-ref (list-ref map2 j) k)))
      (define highest4 (foldl max 0 lst-))
      (define height (list-ref (list-ref map2 i) k))
      (if (> height (foldl min 99 `(,highest ,highest2 ,highest3 ,highest4))) (+ sum 1) sum))))

(writeln (+ inner (- (* size 4) 4)))

(define outer
  (for*/fold ([sum 0]) ([i (in-range 1 (- size 1))] [k (in-range 1 (- size 1))])
    (let ([k- k] [i- i])
      (define left (reverse (take (list-ref map2 i) k)))
      (define right (drop (list-ref map2 i) (+ k 1)))
      (define top
        (reverse (for/list ([j i])
                   (list-ref (list-ref map2 j) k))))
      (define bottom
        (for/list ([j (in-range (+ i 1) size)])
          (list-ref (list-ref map2 j) k)))
      (define height (list-ref (list-ref map2 i) k))
      (define (pred h accum)
        (if (not (cdr accum))
            accum
            (let ([accum (car accum)])
              (if (>= h height) (cons (+ accum 1) #f) (cons (+ accum 1) #t)))))
      (define left- (car (foldl pred (cons 0 #t) left)))
      (define right- (car (foldl pred (cons 0 #t) right)))
      (define top- (car (foldl pred (cons 0 #t) top)))
      (define bottom- (car (foldl pred (cons 0 #t) bottom)))
      (writeln `(,left ,right ,top ,bottom))
      (writeln `(,left- ,right- ,top- ,bottom-))
      (max sum (* left- right- top- bottom-)))))

(writeln outer)
