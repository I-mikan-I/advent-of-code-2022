#lang racket
(require "../utils/text.rkt")
(define input (file->string "day19/input.txt"))

(define-struct blueprint (ore clay obsidian geode)
  #:transparent)

(define (parse input)
  (define lst (string-split input "\n"))
  (for/list ([l lst])
    (match l
      [(regexp
        #rx".*Each ore robot costs (.*) ore. Each clay robot costs (.*) ore. Each obsidian robot costs (.*) ore and (.*) clay. Each geode robot costs (.*) ore and (.*) obsidian."
        (list _ ore clay obsidian1 obsidian2 geode1 geode2))
       (blueprint (string->number ore)
                  (string->number clay)
                  (cons (string->number obsidian1) (string->number obsidian2))
                  (cons (string->number geode1) (string->number geode2)))])))

(define blueprints (parse input))
(define bp1 (first blueprints))

(define (solve blueprint1)
  (match-define (blueprint ore clay (cons obs-ore obs-clay) (cons geo-ore geo-obs)) blueprint1)
  (define dyn (make-hash))
  (let rec ([minutes 24] [bots '(1 0 0 0)] [resources '(0 0 0 0)])
    (cond
      [(<= minutes 0) (list-idx resources 3)]
      [else
       (match-define (list ro rc rob rg) resources)
       (match-define (list bo bc bob bg) bots)
       (define next-resources
         (for/list ([b bots] [r resources])
           (+ b r)))
       (match-define (list no nc nob ng) next-resources)
       (define commands (list (list (- minutes 1) bots next-resources)))
       (if (>= ro ore)
           (set! commands
                 (cons (list (- minutes 1)
                             (list-set bots 0 (+ bo 1))
                             (list-set next-resources 0 (- no ore)))
                       commands))
           (void))
       (if (>= ro clay)
           (set! commands
                 (cons (list (- minutes 1)
                             (list-set bots 1 (+ bc 1))
                             (list-set next-resources 0 (- no clay)))
                       commands))
           (void))
       (if (and (>= ro obs-ore) (>= rc obs-clay))
           (set! commands
                 (cons (list (- minutes 1)
                             (list-set bots 2 (+ bob 1))
                             (list (- no obs-ore) (- nc obs-clay) nob ng))
                       commands))
           (void))
       (if (and (>= ro geo-ore) (>= rob geo-obs))
           (set! commands
                 (cons (list (- minutes 1)
                             (list-set bots 3 (+ bg 1))
                             (list (- no geo-ore) nc (- nob geo-obs) ng))
                       commands))
           (void))
       (set! commands
             (map (lambda (command)
                    (cons (+ (* (first command) (+ 1 (list-ref (second command) 3)))
                             (list-ref (third command) 3))
                          command))
                  (reverse commands)))
       (define result
         (for/fold ([m 0]) ([c commands])
           (if (>= m (car c))
               m
               (max m (if (hash-has-key? dyn (cdr c)) (hash-ref dyn (cdr c)) (apply rec (cdr c)))))))
       (hash-set! dyn `(,minutes ,bots ,resources) result)
       result])))

(define (ex1 blueprints)
  (for/list ([bp blueprints] [i (length blueprints)])
    (writeln i)
    (solve bp)))
