#lang racket

(define (sum s)
  (define ints (map string->number (string-split s "\n")))
  (sequence-fold (lambda (acc next) (acc . + . next)) 0 ints))

(define (max s)
  (define rations (string-split s "\n\n"))
  (foldl (lambda (lst next) (if (> next lst) next lst)) 0 (map sum rations)))

(define (max-three s)
  (define rations (string-split s "\n\n"))
  (foldl (lambda (next three) (if (> next (first three)) (sort (cons next (rest three)) <) three))
         '(0 0 0)
         (map sum rations)))
