#lang racket
(require "../utils/text.rkt")
(define input (file->string "day15/input.txt"))

(define-struct sensor (x y bx by)
  #:transparent)

(define (parse input)
  (for/list ([i (string-split input "\n")])
    (match i
      [(regexp #rx"Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" (list _ x y xb yb))
       (sensor (string->number x) (string->number y) (string->number xb) (string->number yb))])))
(define parsed (parse input))

(define (extract-row lst y-)
  (define dists
    (filter
     (lambda (x) (not (void? x)))
     (for/list ([b lst])
       (define-values (x y bx by) (values (sensor-x b) (sensor-y b) (sensor-bx b) (sensor-by b)))
       (define dist-b (+ (abs (- x bx)) (abs (- y by))))
       (define dist-s (+ (abs (- x x)) (abs (- y y-))))
       (if (> dist-s dist-b) (void) (cons (- x (- dist-b dist-s)) (+ x (- dist-b dist-s)))))))
  (define filtered
    (remove-duplicates
     (let rec- ([todo- dists])
       (define changed
         (let rec ([result '()] [todo todo-] [todo2 todo-])
           (match (cons todo todo2)
             [(cons (cons h1 t1) (cons h2 t2))
              #:when (>= (min (cdr h1) (cdr h2)) (max (car h1) (car h2)))
              (rec result (cons (cons (min (car h1) (car h2)) (max (cdr h1) (cdr h2))) t1) t2)]
             [(cons (cons h1 t1) (cons h2 t2)) (rec result todo t2)]
             [(cons (cons h1 t1) nil) (rec (append result (list h1)) t1 todo-)]
             [(cons nil _) result])))
       (if (equal? changed todo-) changed (rec- changed)))))
  filtered)

(for/sum ([p (extract-row parsed 2000000)]) (- (cdr p) (car p)))

(for/fold ([res '()]) ([y 4000001])
  (define row (extract-row parsed y))
  (define x
    (foldl max
           -1
           (for/list ([f row] [s (cdr row)])
             (if (< (cdr f) (- (car s) 1)) (+ (cdr f) 1) -1))))
  (define res (if (> x -1) (+ (* 4000000 x) y) (void)))
  #:final (not (void? res))
  res)
