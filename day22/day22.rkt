#lang racket
(require "../utils/text.rkt")
(match-define (list mp password) (string-split (file->string "day22/input.txt") "\n\n"))

(define dir->value #hash((R . 0) (D . 1) (L . 2) (U . 3)))

(define (parse input)
  (for/list ([line (string-split input "\n")])
    (for/list ([c (string->list line)])
      (case c
        [(#\.) #t]
        [(#\#) #f]
        [(#\ ) (void)]))))
(define parsed (parse mp))

(define (move1 direction num map pos)
  (define (next current i)
    (match-define (cons r c) current)
    (case direction
      [(L) (cons r (- c i))]
      [(R) (cons r (+ c i))]
      [(U) (cons (- r i) c)]
      [(D) (cons (+ r i) c)]))

  (define (oob r c)
    (or (or (or (< r 0) (< c 0)) (or (>= r (length map)) (>= c (length (list-idx map r)))))
        (void? (list-idx map r c))))

  (for/fold ([current pos]) ([i num])
    (define next-loc (next current 1))
    (define next-loc-
      (cond
        [(oob (car next-loc) (cdr next-loc))
         (for/fold ([current current]) ([i (in-range -1 -1000 -1)])
           (match-define (cons bx by) (next current -1))
           #:break (oob bx by)
           (cons bx by))]
        [else next-loc]))
    #:break (not (list-idx map (car next-loc-) (cdr next-loc-)))
    next-loc-))

(define (parse-command command)
  (define h
    #hash(((R . #\R) . D)
          ((D . #\R) . L)
          ((L . #\R) . U)
          ((U . #\R) . R)
          ((R . #\L) . U)
          ((U . #\L) . L)
          ((L . #\L) . D)
          ((D . #\L) . R)))
  (let rec ([rest (string->list command)] [stack '()] [result '()] [direction 'R])
    (if (empty? rest)
        (begin
          (append result (list (cons direction (string->number (list->string stack))))))
        (match (car rest)
          [(or #\R #\L)
           (let* ([num (string->number (list->string stack))]
                  [result- (append result (list (cons direction num)))]
                  [direction- (hash-ref h (cons direction (car rest)))])
             (rec (cdr rest) '() result- direction-))]
          [digit (rec (cdr rest) (append stack (list digit)) result direction)]))))
(define command (parse-command (string-trim password "\n")))

(define end-pos
  (for/fold ([pos '(0 . 50)]) ([c command])
    (match-define (cons dir num) c)
    (move1 dir num parsed pos)))

(+ (* 1000 (+ 1 (car end-pos))) (* 4 (+ 1 (cdr end-pos))) (hash-ref dir->value (car (last command))))

(define cube%
  (class object%
    (init sz)
    (super-new)
    (field [h
            (hash-copy
             #hash((left . #()) (right . #()) (down . #()) (up . #()) (top . #()) (bottom . #())))])
    (define current 'top)
    (define size sz)

    (define/private (current-set! vec) (hash-set! h current vec))
    (define/private (rotatecw vec)
      (if (vector-empty? vec)
          vec
          (for/vector ([i (vector-length (vector-ref vec 0))])
            (for/vector ([j (in-inclusive-range (- (vector-length vec) 1) 0 -1)])
              (vector-idx vec j i)))))
    (define/private (rotateccw lst) (rotatecw (rotatecw (rotatecw lst))))

    (define/private (rotate-cube direction)
      (case direction
        [(left)
         (set! h
               (make-hash (list (cons 'top (hash-ref h 'left))
                                (cons 'left (hash-ref h 'bottom))
                                (cons 'right (hash-ref h 'top))
                                (cons 'bottom (hash-ref h 'right))
                                (cons 'up (rotateccw (hash-ref h 'up)))
                                (cons 'down (rotatecw (hash-ref h 'down))))))]
        [(right)
         (set! h
               (make-hash (list (cons 'top (hash-ref h 'right))
                                (cons 'left (hash-ref h 'top))
                                (cons 'right (hash-ref h 'bottom))
                                (cons 'bottom (hash-ref h 'left))
                                (cons 'up (rotatecw (hash-ref h 'up)))
                                (cons 'down (rotateccw (hash-ref h 'down))))))]
        [(down)
         (set! h
               (make-hash (list (cons 'top (hash-ref h 'down))
                                (cons 'up (hash-ref h 'top))
                                (cons 'down (rotatecw (rotatecw (hash-ref h 'bottom))))
                                (cons 'bottom (rotatecw (rotatecw (hash-ref h 'up))))
                                (cons 'left (rotateccw (hash-ref h 'left)))
                                (cons 'right (rotatecw (hash-ref h 'right))))))]
        [(up)
         (set! h
               (make-hash (list (cons 'top (hash-ref h 'up))
                                (cons 'up (rotatecw (rotatecw (hash-ref h 'bottom))))
                                (cons 'down (hash-ref h 'top))
                                (cons 'bottom (rotatecw (rotatecw (hash-ref h 'down))))
                                (cons 'left (rotatecw (hash-ref h 'left)))
                                (cons 'right (rotateccw (hash-ref h 'right))))))]))

    (define/public (move direction num pos)
      (define (next current i)
        (match-define (cons r c) current)
        (case direction
          [(L) (cons r (- c i))]
          [(R) (cons r (+ c i))]
          [(U) (cons (- r i) c)]
          [(D) (cons (+ r i) c)]))

      (define (oob? r c)
        (or (or (or (< r 0) (< c 0))
                (or (>= r (vector-length (hash-ref h current)))
                    (>= c (vector-length (vector-idx (hash-ref h current) r)))))
            (void? (vector-idx (hash-ref h current) r c))))
      (define (blocked? r c)
        (not (cdr (vector-idx (hash-ref h current) r c))))

      (for/fold ([currentpos pos]) ([i num])
        (match-define (cons r c) currentpos)
        (define next-loc (next currentpos 1))
        (define next-loc-
          (cond
            [(oob? (car next-loc) (cdr next-loc))
             (case direction
               [(L)
                (rotate-cube 'left)
                (if (blocked? r (- size 1))
                    (begin
                      (rotate-cube 'right)
                      currentpos)
                    (cons r (- size 1)))]
               [(R)
                (rotate-cube 'right)
                (if (blocked? r 0)
                    (begin
                      (rotate-cube 'left)
                      currentpos)
                    (cons r 0))]
               [(D)
                (rotate-cube 'down)
                (if (blocked? 0 c)
                    (begin
                      (rotate-cube 'up)
                      currentpos)
                    (cons 0 c))]
               [(U)
                (rotate-cube 'up)
                (if (blocked? (- size 1) c)
                    (begin
                      (rotate-cube 'down)
                      currentpos)
                    (cons (- size 1) c))])]
            [else next-loc]))
        #:break (blocked? (car next-loc-) (cdr next-loc-))
        next-loc-))

    (define/public (parse lst-)

      (define (oob? r c)
        (or (or (or (< r 0) (< c 0))
                (or (>= r (vector-length lst)) (>= c (vector-length (vector-idx lst r)))))
            (void? (vector-idx lst r c))))
      (define lst
        (for/vector ([row lst-] [i (length lst-)])
          (for/vector ([entry row] [j (length row)])
            (if (not (void? entry)) (cons (cons i j) entry) (void)))))

      (define begin (index-where (vector->list (vector-idx lst 0)) (compose not void?)))
      (define-values (r c) (values 0 begin))
      (define (update-side rown column)
        (current-set! (for/vector ([i size])
                        (define row (vector-idx lst (+ rown i)))
                        (vector-take (vector-drop row column) size))))
      (let rec ([row 0] [column c] [backstep (void)])
        (update-side row column)
        (define next-dir
          (cond
            [(and (vector-empty? (hash-ref h 'left)) (not (oob? row (- column size))))
             (list (cons row (- column size)) 'left 'right)]
            [(and (vector-empty? (hash-ref h 'right)) (not (oob? row (+ column size))))
             (list (cons row (+ column size)) 'right 'left)]
            [(and (vector-empty? (hash-ref h 'up)) (not (oob? (- row size) column)))
             (list (cons (- row size) column) 'up 'down)]
            [(and (vector-empty? (hash-ref h 'down)) (not (oob? (+ row size) column)))
             (list (cons (+ row size) column) 'down 'up)]
            [else (void)]))
        (if (void? next-dir)
            (if (void? backstep) (void) (rotate-cube backstep))
            (match-let ([(list (cons nr nc) dir backdir) next-dir])
              (rotate-cube dir)
              (rec nr nc backdir)
              (rec row column backstep)))))))
(define cube (new cube% [sz 50]))
(send cube parse parsed)
(define end-pos2
  (for/fold ([pos '(0 . 0)]) ([c command])
    (match-define (cons dir num) c)
    (send cube move dir num pos)))

(let* ([endstate (get-field h cube)]
       [endpos (vector-idx (hash-ref endstate 'top) (car end-pos2) (cdr end-pos2))])
  (match-define (cons (cons x y) _) endpos)
  (+ (* 1000 (+ x 1)) (* 4 (+ y 1)) 3))
