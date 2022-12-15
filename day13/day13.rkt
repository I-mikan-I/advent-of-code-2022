#lang racket
(require "../utils/text.rkt")
(define input (file->string "day13/input.txt"))

(define (parse s)
  (define-values (res _)
    (let rec ([i 0] [str (drop (string->list s) 1)] [saved '()] [result '()])
      (define char (list-ref str i))
      (case char
        [(#\[)
         (define-values (result- next) (rec 0 (drop str (+ i 1)) '() '()))
         (rec (+ i next 1) str saved (append result (list result-)))]
        [(#\])
         (if (not (empty? saved))
             (values (append result (list (string->number (list->string saved)))) (+ i 1))
             (values result (+ i 1)))]
        [(#\,)
         (if (not (empty? saved))
             (rec (+ i 1) str '() (append result (list (string->number (list->string saved)))))
             (rec (+ i 1) str saved result))]
        [else (rec (+ i 1) str (append saved (list char)) result)])))
  res)

(define (compare left right)
  (define res
    (match (cons left right)
      [(cons '() '()) (void)]
      [(cons '() rest) #t]
      [(cons rest '()) #f]
      [(cons _ _)
       (for/last ([l left] [r right])
         (define res
           (match (cons l r)
             [(cons '() rest) #t]
             [(cons rest '()) #f]
             [(cons (cons _ _) (cons _ _)) (compare l r)]
             [(cons (cons _ _) int) (compare l (list r))]
             [(cons int (cons _ _)) (compare (list l) r)]
             [(cons int int2)
              (cond
                [(< int int2) #t]
                [(> int int2) #f]
                [else (void)])]))
         #:final (not (void? res))
         res)]))
  (define res2
    (if (and (void? res) (not (equal? (length left) (length right))))
        (cond
          [(< (length left) (length right)) #t]
          [(> (length left) (length right)) #f])
        res))
  res2)

(define pairs (string-split input "\n\n"))

(for/sum [(i (in-range 1 (+ 1 (length pairs)))) (pair pairs)]
         (define p (string-split pair "\n"))
         (define-values (l r) (values (parse (first p)) (parse (second p))))
         (if (compare l r) i 0))

(define all
  (map parse
       (for/fold ([res '()]) ([p pairs])
         (append res (string-split p "\n")))))

(define all2 (append '(((2))) '(((6))) all))

(define all3 (sort all2 compare))

(* (+ (index-of all3 '((2))) 1) (+ (index-of all3 '((6))) 1))
