#lang rosette/safe

(define-struct blueprint (ore clay obsidian geode)
  #:transparent)

(define blueprints
  (list (blueprint 2 4 '(4 . 20) '(3 . 14))
        (blueprint 3 3 '(2 . 20) '(2 . 20))
        (blueprint 3 3 '(3 . 16) '(3 . 9))
        (blueprint 3 4 '(2 . 15) '(2 . 13))
        (blueprint 2 4 '(4 . 16) '(3 . 13))
        (blueprint 3 4 '(2 . 14) '(3 . 14))
        (blueprint 3 4 '(4 . 6) '(2 . 20))
        (blueprint 3 4 '(4 . 5) '(4 . 8))
        (blueprint 3 4 '(3 . 19) '(3 . 8))
        (blueprint 2 3 '(2 . 14) '(3 . 8))
        (blueprint 2 4 '(3 . 19) '(4 . 13))
        (blueprint 2 4 '(4 . 20) '(4 . 18))
        (blueprint 4 4 '(2 . 16) '(4 . 16))
        (blueprint 2 4 '(3 . 20) '(2 . 16))
        (blueprint 4 4 '(3 . 11) '(3 . 8))
        (blueprint 4 3 '(4 . 19) '(4 . 12))
        (blueprint 2 4 '(2 . 20) '(3 . 15))
        (blueprint 4 4 '(4 . 15) '(4 . 20))
        (blueprint 4 3 '(4 . 15) '(4 . 9))
        (blueprint 3 3 '(2 . 7) '(2 . 9))
        (blueprint 2 3 '(3 . 14) '(3 . 19))
        (blueprint 4 3 '(3 . 17) '(3 . 13))
        (blueprint 3 4 '(3 . 18) '(4 . 19))
        (blueprint 3 3 '(3 . 17) '(2 . 13))
        (blueprint 4 4 '(2 . 15) '(3 . 16))
        (blueprint 4 3 '(3 . 15) '(2 . 13))
        (blueprint 4 4 '(4 . 18) '(4 . 9))
        (blueprint 4 4 '(4 . 7) '(2 . 19))
        (blueprint 4 4 '(4 . 15) '(4 . 17))
        (blueprint 4 4 '(4 . 9) '(4 . 16))))

(define-symbolic build-orebot boolean? #:length 32)
(define-symbolic build-claybot boolean? #:length 32)
(define-symbolic build-obsidianbot boolean? #:length 32)
(define-symbolic build-geodebot boolean? #:length 32)

(define (minimal-bp blueprint- minutes)
  (define ore (blueprint-ore blueprint-))
  (define clay (blueprint-clay blueprint-))
  (define obsidian (blueprint-obsidian blueprint-))
  (define geode (blueprint-geode blueprint-))
  (define (rec minutes bots resources)
    (cond
      [(< minutes 0) (list-ref resources 3)]
      [else
       (define ro (first resources))
       (define rc (second resources))
       (define rob (third resources))
       (define rg (fourth resources))

       (define bo (first bots))
       (define bc (second bots))
       (define bob (third bots))
       (define bg (fourth bots))
       (define next-resources (list (+ ro bo) (+ rc bc) (+ rob bob) (+ bg rg)))
       (define build-orebot-v (list-ref build-orebot minutes))
       (define build-claybot-v (list-ref build-claybot minutes))
       (define build-obsidianbot-v (list-ref build-obsidianbot minutes))
       (define build-geodebot-v (list-ref build-geodebot minutes))
       (assert (=> build-orebot-v (! (or build-claybot-v build-obsidianbot-v build-geodebot-v))))
       (assert (=> build-claybot-v (! (or build-orebot-v build-obsidianbot-v build-geodebot-v))))
       (assert (=> build-obsidianbot-v (! (or build-claybot-v build-orebot-v build-geodebot-v))))
       (assert (=> build-geodebot-v (! (or build-claybot-v build-obsidianbot-v build-orebot-v))))
       (define commands
         (cond
           [(and build-orebot-v (>= ro ore))
            (list (- minutes 1)
                  (list-set bots 0 (+ bo 1))
                  (list-set next-resources 0 (- (list-ref next-resources 0) ore)))]
           [(and build-claybot-v (>= ro clay))
            (list (- minutes 1)
                  (list-set bots 1 (+ bc 1))
                  (list-set next-resources 0 (- (list-ref next-resources 0) clay)))]
           [(and build-obsidianbot-v (>= ro (car obsidian)) (>= rc (cdr obsidian)))
            (list (- minutes 1)
                  (list-set bots 2 (+ bob 1))
                  (list (- (list-ref next-resources 0) (car obsidian))
                        (- (list-ref next-resources 1) (cdr obsidian))
                        (+ rob bob)
                        (+ bg rg)))]
           [(and build-geodebot-v (>= ro (car geode)) (>= rob (cdr geode)))
            (list (- minutes 1)
                  (list-set bots 3 (+ bg 1))
                  (list (- (+ ro bo) (car geode)) (+ rc bc) (- (+ rob bob) (cdr geode)) (+ bg rg)))]
           [else (list (- minutes 1) bots next-resources)]))
       (apply rec commands)]))
  (rec minutes '(1 0 0 0) '(0 0 0 0)))

(foldl +
       0
       (let rec ([i 0] [results '()])
         (cond
           [(>= i (length blueprints)) results]
           [else
            (clear-vc!)
            (rec (+ i 1)
                 (cons (let ([model (minimal-bp (list-ref blueprints i) 23)])
                         (define right
                           (let find-right ([i 1])
                             (if (sat? (solve (assert (> model i)))) (find-right (* i 2)) i)))
                         (let rec2 ([left (quotient right 2)]
                                    [high right]
                                    [middle (- right 1 (quotient right 4))])
                           (define res (solve (assert (> model middle))))
                           (cond
                             [(and (unsat? res) (<= (- middle left) 1))
                              (begin
                                (printf "got result ~a for i = ~a\n" middle i)
                                (* middle (+ 1 i)))]
                             [(unsat? res) (rec2 left middle (+ left (quotient (- middle left) 2)))]
                             [else (rec2 middle high (+ middle (ceiling (/ (- high middle) 2))))])))
                       results))])))
(clear-vc!)
(foldl *
       1
       (let rec ([i 0] [results '()])
         (cond
           [(>= i 3) results]
           [else
            (clear-vc!)
            (rec (+ i 1)
                 (cons (let ([model (minimal-bp (list-ref blueprints i) 31)])
                         (define right
                           (let find-right ([i 1])
                             (if (sat? (solve (assert (> model i)))) (find-right (* i 2)) i)))
                         (let rec2 ([left (quotient right 2)]
                                    [high right]
                                    [middle (- right 1 (quotient right 4))])
                           (define res (solve (assert (> model middle))))
                           (cond
                             [(and (unsat? res) (<= (- middle left) 1))
                              (begin
                                (printf "got result ~a for i = ~a\n" middle i)
                                middle)]
                             [(unsat? res) (rec2 left middle (+ left (quotient (- middle left) 2)))]
                             [else (rec2 middle high (+ middle (ceiling (/ (- high middle) 2))))])))
                       results))])))
