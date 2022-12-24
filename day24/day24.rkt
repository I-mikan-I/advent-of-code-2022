#lang racket
(require "../utils/text.rkt")
(define input (file->string "day24/input.txt"))

(define left (make-hash))
(define right (make-hash))
(define down (make-hash))
(define up (make-hash))
(define nr (make-hash))
(define nl (make-hash))
(define nu (make-hash))
(define nd (make-hash))
(define width #f)
(define height #f)

(define (parse input)
  (let* ([lines (map string->list (string-split input "\n"))]
         [rows (- (length lines) 2)]
         [cols (- (length (list-idx lines 0)) 2)]
         [valley (map (lambda (row) (take (drop row 1) cols)) (take (drop lines 1) rows))])
    (set! width cols)
    (set! height rows)
    (for ([row valley] [i (length valley)])
      (for ([char row] [j (length row)])
        (case char
          [(#\<) (hash-set! left (cons i j) #t)]
          [(#\>) (hash-set! right (cons i j) #t)]
          [(#\^) (hash-set! up (cons i j) #t)]
          [(#\v) (hash-set! down (cons i j) #t)]
          [(#\.) (void)]))))
  (set! nr right)
  (set! nl left)
  (set! nu up)
  (set! nd down))

(define (advance!)
  (set! left nl)
  (set! right nr)
  (set! down nd)
  (set! up nu)
  (set! nl
        (for/hash ([pos (hash-keys nl)])
          (define next-pos (cons (car pos) (- (cdr pos) 1)))
          (if (< (cdr next-pos) 0) (values (cons (car pos) (- width 1)) #t) (values next-pos #t))))
  (set! nr
        (for/hash ([pos (hash-keys nr)])
          (define next-pos (cons (car pos) (+ (cdr pos) 1)))
          (if (>= (cdr next-pos) width) (values (cons (car pos) 0) #t) (values next-pos #t))))
  (set! nd
        (for/hash ([pos (hash-keys nd)])
          (define next-pos (cons (+ (car pos) 1) (cdr pos)))
          (if (>= (car next-pos) height) (values (cons 0 (cdr pos)) #t) (values next-pos #t))))
  (set! nu
        (for/hash ([pos (hash-keys nu)])
          (define next-pos (cons (- (car pos) 1) (cdr pos)))
          (if (< (car next-pos) 0) (values (cons (- height 1) (cdr pos)) #t) (values next-pos #t)))))

(define (freenr? r c)
  (define (oob? r c)
    (and (not (member (cons r c) `((-1 . 0) ,(cons height (- width 1)))))
         (or (< r 0) (>= r height) (< c 0) (>= c width))))
  (cond
    [(oob? r c) #f]
    [else
     (define pos (cons r c))
     (define res
       (not (or (hash-has-key? nl pos)
                (hash-has-key? nr pos)
                (hash-has-key? nd pos)
                (hash-has-key? nu pos))))
     res]))

(define (walk start end)
  (let rec ([queue (list start)] [i 0])
    (cond
      [(member end queue) i]
      [(empty? queue) (error "no way possible")]
      [else
       (define next-queue
         (remove-duplicates (for/fold ([res '()]) ([pos queue])
                              (match-define (cons r c) pos)
                              (define moves
                                (for/list ([move (list (cons (- r 1) c)
                                                       (cons (+ r 1) c)
                                                       (cons r (- c 1))
                                                       (cons r (+ c 1))
                                                       (cons r c))]
                                           #:unless (not (freenr? (car move) (cdr move))))
                                  move))
                              (append res moves))))
       (advance!)
       (rec next-queue (+ i 1))])))

(define (ex1)
  (parse input)
  (advance!)
  (writeln (walk '(-1 . 0) (cons height (- width 1)))))

(define (ex2)
  (set! left (make-hash))
  (set! right (make-hash))
  (set! down (make-hash))
  (set! up (make-hash))
  (parse input)
  (advance!)
  (define path1 (walk '(-1 . 0) (cons height (- width 1))))
  (define path2 (walk (cons height (- width 1)) '(-1 . 0)))
  (define path3 (walk '(-1 . 0) (cons height (- width 1))))
  (+ path1 path2 path3))

(ex1)
(ex2)
