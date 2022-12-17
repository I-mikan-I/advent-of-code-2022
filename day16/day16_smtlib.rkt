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

(define (gen-state i h)
  (define nodes (hash-values h))
  (let rec ([location-decls '()] [action-decls '()] [nodes nodes])
    (match-define (cons head tail) nodes)
    (define location `((declare-const ,(string->symbol (format "s~a-~a" i (valve-name head))) Bool)))
    (define actions
      (append
       (for/list ([tunnel (valve-tunnels head)])
         `(declare-const ,(string->symbol (format "s~a-~a-MOVE-~a" i (valve-name head) tunnel)) Bool))
       (if ((valve-rate head) . > . 0)
           `((declare-const ,(string->symbol (format "s~a-~a-OPEN" i (valve-name head))) Bool))
           '())))

    (if (empty? tail)
        (list (append location-decls location) (append action-decls actions))
        (rec (append location-decls location) (append action-decls actions) tail))))

(define (gen-constraints-state i state h)
  (define locations (first state))
  (define actions (second state))
  (define least-one-loc `(assert (or ,@(map (lambda (decl) (second decl)) locations))))
  (define least-one-action `(assert (or ,@(map (lambda (decl) (second decl)) actions))))
  (define action-implication
    `(assert (and ,@(for/list ([a actions])
                      (match a
                        [(list decl
                               (and act
                                    (app symbol->string
                                         (regexp #rx"s([^-]*)-([^-]*)-(?:.*)" (list _ n node))))
                               srt)
                         `(=> ,act ,(string->symbol (format "s~a-~a" n node)))])))))

  (define most-one-loc
    `(assert (and ,@(for/list ([l locations])
                      `(=> ,(second l) (not (or ,@(map second (remove l locations)))))))))
  (define most-one-action
    `(assert (and ,@(for/list ([a actions])
                      `(=> ,(second a) (not (or ,@(map second (remove a actions)))))))))
  (list least-one-loc least-one-action most-one-action action-implication most-one-loc))

(define (gen-interstate-constaints i1 i2 h)
  (let rec ([constraints '()] [nodes (hash-values h)])
    (match-define (cons head tail) nodes)
    (define moves
      (for/list ([tunnel (valve-tunnels head)])
        `(assert (=> ,(string->symbol (format "s~a-~a-MOVE-~a" i1 (valve-name head) tunnel))
                     ,(string->symbol (format "s~a-~a" i2 tunnel))))))
    (define opens
      (if (> (valve-rate head) 0)
          `((assert (=> ,(string->symbol (format "s~a-~a-OPEN" i1 (valve-name head)))
                        ,(string->symbol (format "s~a-~a" i2 (valve-name head))))))
          '()))
    (if (empty? tail) (append constraints moves opens) (rec (append constraints moves opens) tail))))

(define state0 (gen-state 0 h))

(define (get-count-funs h)
  (define unblocked (filter (lambda (v) (> (valve-rate v) 0)) (hash-values h)))
  (for/list ([u unblocked])
    (let* ([rate (valve-rate u)]
           [name (valve-name u)]
           [func-name (string->symbol (format "count-~a" name))])
      `(define-fun ,func-name
                   ()
                   Int
                   ,(let rec ([i 29])
                      (if (< i 0)
                          0
                          `(ite ,(string->symbol (format "s~a-~a-OPEN" i name))
                                ,(* (- 29 i) rate)
                                ,(rec (- i 1)))))))))

(define (get-sum-fun h)
  (define unblocked (filter (lambda (v) (> (valve-rate v) 0)) (hash-values h)))
  `(define-fun sum
               ()
               Int
               (+ ,@(map (lambda (v) (string->symbol (format "count-~a" (valve-name v))))
                         unblocked))))

(define (write-file)
  (with-output-to-file #:exists 'truncate
                       "day16/volcano.smt2"
                       (lambda ()
                         (writeln '(set-logic QF_UFLIA))
                         (for ([i 30])
                           (let ([state (gen-state i h)])
                             (for-each writeln (first state))
                             (for-each writeln (second state))))
                         (for ([i 30])
                           (for-each writeln (gen-constraints-state i (gen-state i h) h)))
                         (for ([i 29] [j (in-range 1 30)])
                           (let* ([interstate-constraints (gen-interstate-constaints i j h)])
                             (for-each writeln interstate-constraints)))
                         (for-each writeln (get-count-funs h))
                         (writeln (get-sum-fun h))
                         (writeln '(assert s0-AA))
                         (writeln '(assert (>= sum 1767)))
                         (writeln '(check-sat))
                         (writeln '(get-model))
                         (writeln '(get-value (sum))))))

(write-file)