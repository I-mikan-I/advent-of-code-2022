#lang racket
(require "../utils/text.rkt")
(require racket/hash)
(define input (file->string "day23/input.txt"))

(define dirs null)
(shared ([directions (append (list (cons '(-1 . 0) (list '(-1 . 0) '(-1 . 1) '(-1 . -1)))
                                   (cons '(1 . 0) (list '(1 . 0) '(1 . 1) '(1 . -1)))
                                   (cons '(0 . -1) (list '(0 . -1) '(-1 . -1) '(1 . -1)))
                                   (cons '(0 . 1) (list '(0 . 1) '(-1 . 1) '(1 . 1))))
                             directions)])
  (begin
    (set! dirs directions))
  (void))

(define h
  (for/fold ([current (hash)])
            ([row (string-split input "\n")] [i (length (string-split input "\n"))])
    (hash-union current
                (for/hash ([c row] [j (string-length row)] #:unless (equal? c #\.))
                  (values (cons i j) #t)))))

(define (ex1 h round ex2?)
  (let rec ([h h] [i 0] [dirs dirs])
    (cond
      [(>= i round) h]
      [else
       (define proposals (make-hash))
       (for ([elve (hash-keys h)])
         (if (for/and ([direction (take dirs 4)])
               (match-define (cons movement checklist) direction)
               (for/and ([check checklist])
                 (define pos (cons (+ (car elve) (car check)) (+ (cdr elve) (cdr check))))
                 (not (hash-has-key? h pos))))
             (void)
             (for ([direction (take dirs 4)])
               (match-define (cons movement checklist) direction)
               (define pos (cons (+ (car elve) (car movement)) (+ (cdr elve) (cdr movement))))
               (define mov
                 (for/and ([check checklist])
                   (define pos (cons (+ (car elve) (car check)) (+ (cdr elve) (cdr check))))
                   (not (hash-has-key? h pos))))
               #:final mov
               (if mov
                   (if (hash-has-key? proposals pos)
                       (hash-set! proposals pos #f)
                       (hash-set! proposals pos elve))
                   (void)))))
       (define elveMovements (hash-map/copy proposals (lambda (k v) (values v k))))
       (define next-h
         (hash-map/copy
          h
          (lambda (k v)
            (if (hash-has-key? elveMovements k) (values (hash-ref elveMovements k) v) (values k v)))))
       (if (and ex2? (equal? h next-h))
           (printf "no elve moved at round: ~a\n" (+ i 1))
           (rec next-h (+ i 1) (cdr dirs)))])))

(define last (ex1 h 10 #f))

(define-values (sx sy lx ly)
  (for/fold ([sx 0] [sy 0] [lx 0] [ly 0]) ([elve (hash-keys last)])
    (match-define (cons x y) elve)
    (values (min sx x) (min sy y) (max lx x) (max ly y))))

(- (* (+ (abs sx) lx 1) (+ (abs sy) ly 1)) (length (hash-keys last)))

(define _ (ex1 h 1500 #t))
