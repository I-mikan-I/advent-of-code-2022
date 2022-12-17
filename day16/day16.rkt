#lang racket
(require "../utils/text.rkt")
(define input (file->string "day16/input.txt"))

(define h (make-hash))
(define-struct valve (name rate tunnels)
  #:transparent)

(define (parse input)
  (for ([s (string-split input "\n")])
    (match s
      [(regexp
        #rx"Valve (.*) has flow rate=(.*); (?:tunnel|tunnels) (?:lead|leads) to (?:valves|valve) (.*)"
        (list _ name flow edges))
       (define v
         (valve name (string->number flow) (map ((curryr string-trim) " ") (string-split edges ","))))
       (hash-set! h name v)])))

(parse input)

(define rates
  (make-immutable-hash (for/list ([valve (hash-values h)])
                         (cons (valve-name valve) (valve-rate valve)))))

(define (find-preds pred node)
  (if (hash-has-key? pred node)
      (cons (hash-ref pred node) (find-preds pred (hash-ref pred node)))
      '()))

(define (ex1 h)
  (define dyn (make-hash))
  (define (rec time set at)
    (cond
      [(equal? time 1) 0]
      [(>= (length set) 50) 0]
      [else
       (set! set (sort set string<?))
       (define cv (hash-ref h at))
       (define open
         (if (and (> (valve-rate cv) 0) (not (member at set)))
             (list (- time 1) (cons at set) at)
             '()))
       (define adj
         (for/list ([nv (valve-tunnels cv)])
           (list (- time 1) set nv)))
       (define results
         (append (if (empty? open)
                     '()
                     (list (+ (* (valve-rate cv) (- time 1))
                              (if (hash-has-key? dyn open)
                                  (begin
                                    (hash-ref dyn open))
                                  (apply rec open)))))
                 (for/list ([command adj])
                   (if (hash-has-key? dyn command) (hash-ref dyn command) (apply rec command)))))
       (hash-set! dyn `(,time ,set ,at) (foldl max 0 results))
       (foldl max 0 results)]))
  (rec 30 '() "AA"))

(define flows
  (make-hash (for/list ([vals (hash-values h)])
               (cons (valve-name vals) (valve-rate vals)))))
(define (flows-excl time set)
  (define sorted (sort (hash->list flows) (lambda (kv kv2) (> (cdr kv) (cdr kv2)))))
  (define sorted- (filter (lambda (m) (not (hash-has-key? set (car m)))) sorted))
  (for/sum ([t (in-range time 0 -1)] [s sorted-]) (* (- t 1) (cdr s))))

(define APSP
  (let ([dists (make-hash)])
    (for ([u (hash-values h)])
      (for ([v (hash-values h)])
        (if (member (valve-name v) (valve-tunnels u))
            (hash-set! dists (cons (valve-name u) (valve-name v)) 1)
            (hash-set! dists (cons (valve-name u) (valve-name v)) 99999))))
    (for ([r (hash-keys h)])
      (for ([u (hash-keys h)])
        (for ([v (hash-keys h)])
          (if (> (hash-ref dists (cons u v))
                 (+ (hash-ref dists (cons u r)) (hash-ref dists (cons r v))))
              (hash-set! dists (cons u v) (+ (hash-ref dists (cons u r)) (hash-ref dists (cons r v))))
              (void)))))
    dists))

(define (ex2 h time)
  (define dyn (make-hash))
  (define (bound steps set)
    (define set- (sort (map ((curry hash-ref) rates) set) <))
    (for/sum ([i (in-range (car steps) 0 -3)] [j (in-range (cdr steps) 0 -3)])
             #:break (< (length set-) 1)
             (match set-
               [(list fst snd rest ...)
                (set! set- rest)
                (+ (* i fst) (* j snd))]
               [(list fst rest ...) (* i fst)])))

  (define (rec steps set at)
    (cond
      [(empty? set) (cons 0 '())]
      [else
       (define targets
         (if (> (cdr steps) 0)
             (map (lambda (pair)
                    (let ([p1 (cons (car at) (first pair))] [p2 (cons (cdr at) (second pair))])
                      (list (cons (- (car steps) (hash-ref APSP p1) 1) (first pair))
                            (cons (- (cdr steps) (hash-ref APSP p2) 1) (second pair)))))
                  (filter (lambda (lst) (not (equal? (first lst) (second lst))))
                          (cartesian-product set set)))
             (map (lambda (single)
                    (let ([p1 (cons (car at) (first single))])
                      (list (cons (- (car steps) (hash-ref APSP p1) 1) (first single))
                            (cons -1 "AA"))))
                  (combinations set 1))))
       (define filtered
         (filter (lambda (lst) (or (>= (car (first lst)) 0) (>= (car (second lst)) 0))) targets))
       (define commands
         (map (lambda (command) (cons (bound (first command) (second command)) command))
              (map (lambda (command)
                     (if (> (cdr (first command)) (car (first command)))
                         (list (cons (cdr (first command)) (car (first command)))
                               (second command)
                               (cons (cdr (third command)) (car (third command))))
                         command))
                   (for/list ([f filtered])
                     (match f
                       [(list (cons d1 t1) (cons d2 t2))
                        #:when (< d1 0)
                        (list (cons d2 0) (sort (remove t2 set) string<?) (cons t2 "AA"))]
                       [(list (cons d1 t1) (cons d2 t2))
                        #:when (< d2 0)
                        (list (cons d1 0) (sort (remove t1 set) string<?) (cons t1 "AA"))]
                       [_
                        (list (cons (car (first f)) (car (second f)))
                              (sort (remove (cdr (first f)) (remove (cdr (second f)) set)) string<?)
                              (cons (cdr (first f)) (cdr (second f))))])))))
       (define result
         (for/fold ([m (cons 0 '())]) ([command commands])
           (let* ([c (cdr command)]
                  [res2 (+ (* (valve-rate (hash-ref h (cdr (third c)))) (cdr (first c)))
                           (* (valve-rate (hash-ref h (car (third c)))) (car (first c))))])
             (if (>= (car m) (+ res2 (car command)))
                 m
                 (let* ([res (if (hash-has-key? dyn c) (hash-ref dyn c) (apply rec c))]
                        [res3 (+ (+ (* (valve-rate (hash-ref h (cdr (third c)))) (cdr (first c)))
                                    (* (valve-rate (hash-ref h (car (third c)))) (car (first c))))
                                 (car res))])
                   (if (> (car m) res3) m (cons res3 (cons (third c) (cdr res)))))))))

       (hash-set! dyn `(,steps ,set ,at) result)
       result]))
  (rec `(,time . ,time)
       (for/list ([v (hash-values h)] #:unless (equal? 0 (valve-rate v)))
         (valve-name v))
       '("AA" . "AA")))

(ex1 h)
(ex2 h 26)