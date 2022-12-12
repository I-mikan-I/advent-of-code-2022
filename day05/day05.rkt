#lang racket

(require "../utils/text.rkt")

(define (read-crates s)
  (let rec ([lst '()] [rest (string-append s " ")])
    (if (string-prefix? rest "[")
        (let ([crate (string-ref rest 1)] [next (substring rest 4)])
          (rec (append lst (list crate)) next))
        (if (string-contains? rest "[") (rec (append lst (list (void))) (substring rest 4)) lst))))

(struct move (from to num) #:transparent)

(define (read-moves s)
  (let rec ([lst (string-split s "\n")] [res '()])
    (if (empty? lst)
        res
        (match (string->list (car lst))
          [(list #\m #\o #\v #\e #\  num ... #\  #\f #\r #\o #\m #\  from ... #\  #\t #\o #\  to ...)
           (define-values (n f t)
             (values (string->number (list->string num))
                     (string->number (list->string from))
                     (string->number (list->string to))))
           (rec (cdr lst) (append res (list (move f t n))))]))))

(define (ex02 s)
  (define lst (string-split s " 1   2   3   4   5   6   7   8   9 \n\n"))
  (match lst
    [(list init moves)
     (define init-crates (map read-crates (string-split init "\n")))
     (define stacks
       (for/list ([i '(0 1 2 3 4 5 6 7 8)])
         (remove* (list (void))
                  (map (lambda (lst) (if (>= i (length lst)) (void) (list-ref lst i))) init-crates))))
     (define moves-list (read-moves moves))

     (list->string (map first
                        (foldl (lambda (move crates)
                                 (let* ([from-list (- (move-from move) 1)]
                                        [to-list (- (move-to move) 1)]
                                        [num (move-num move)]
                                        [from-new (drop (list-ref crates from-list) num)]
                                        [to-new (append (take (list-ref crates from-list) num)
                                                        (list-ref crates to-list))])
                                   (list-set (list-set crates from-list from-new) to-list to-new)))
                               stacks
                               moves-list)))]))

(define input (file->string "day05/input.txt"))
