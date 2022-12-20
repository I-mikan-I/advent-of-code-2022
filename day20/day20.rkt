#lang racket
(require "../utils/text.rkt")
(define input (file->string "day20/input.txt"))

(define lst (map (lambda (n) (cons #f n)) (map string->number (string-split input "\n"))))

(define (ex1 lst)
  (let rec ([i 0])
    (cond
      [(>= i (length lst)) (void)]
      [else
       (match-define (cons moved offset) (list-ref lst i))
       (cond
         [moved (rec (+ i 1))]
         [else
          (let* ([idx (modulo (+ offset i) (- (length lst) 1))]
                 [removed (append (take lst i) (drop lst (+ i 1)))])
            (define new-list (append (take removed idx) (list (cons #t offset)) (drop removed idx)))
            (set! lst new-list)
            (cond
              [(<= idx i) (rec (+ i 1))]
              [else (rec i)]))])]))

  (set! lst (map cdr lst))
  (define zero (index-of lst 0))
  (for/sum ([i '(1000 2000 3000)]) (list-ref lst (modulo (+ zero i) (length lst)))))

(ex1 lst)

(define (ex2 lst)
  (let rec- ([j 0])
    (if (< j 10)
        (begin
          (let rec ([i 0] [orig-pos 0])
            (cond
              [(>= i (vector-length lst)) (void)]
              [else
               (match-define (list original moved offset) (vector-ref lst i))
               (cond
                 [(or #f (not (equal? original orig-pos))) (rec (+ i 1) orig-pos)]
                 [else
                  (let* ([idx (modulo (+ offset i) (- (vector-length lst) 1))]
                         [removed (vector-append (vector-take lst i) (vector-drop lst (+ i 1)))])
                    (define new-list
                      (vector-append (vector-take removed idx)
                                     (vector (list original #t offset))
                                     (vector-drop removed idx)))
                    (set! lst new-list)
                    (rec 0 (+ orig-pos 1)))])]))
          (println j)
          (rec- (+ j 1)))
        lst))
  (set! lst (vector->list lst))
  (set! lst (map third lst))
  (define zero (index-of lst 0))
  (for/sum ([i '(1000 2000 3000)]) (list-ref lst (modulo (+ zero i) (length lst)))))

(ex2 (for/vector ([l lst] [i (length lst)])
       (list i (car l) (* 811589153 (cdr l)))))
