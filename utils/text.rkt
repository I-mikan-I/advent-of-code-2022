#lang racket

(provide file->string
         deconstruct
         map-deconstruct
         for/fold/list
         list-idx-set
         vector-idx
         list-idx)

(define (file->string path)
  (port->string (open-input-file path)))

(define-syntax deconstruct
  (syntax-rules ()
    [(deconstruct body str) (list->string (body (string->list str)))]
    [(deconstruct body str box-fn) (box-fn (body (string->list str)))]))

(define-syntax map-deconstruct
  (syntax-rules ()
    [(map-deconstruct body lst) (map (lambda (str) (deconstruct body str)) lst)]
    [(map-deconstruct body lst box-fn) (map (lambda (str) (deconstruct body str box-fn)) lst)]))

(define-syntax-rule (for/fold/list ((folder foldee) ...) (iter ...) body)
  (for/fold ([folder foldee] ... [results '()] #:result results) (iter ...)
    (let-values ([(folder ... result) body]) (values folder ... (append results (list result))))))

(define-syntax list-idx
  (syntax-rules ()
    [(list-idx lst index) (list-ref lst index)]
    [(list-idx lst index ... last) (list-ref (list-idx lst index ...) last)]))

(define-syntax list-idx-set
  (syntax-rules ()
    [(list-idx-set lst pos value) (list-set lst pos value)]
    [(list-idx-set lst first pos ... value)
     (list-set lst first (list-idx-set (list-ref lst first) pos ... value))]))

(define-syntax vector-idx
  (syntax-rules ()
    [(vector-idx vec index) (vector-ref vec index)]
    [(vector-idx vec index ... last) (vector-ref (vector-idx vec index ...) last)]))
