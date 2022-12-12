#lang racket

(provide file->string
         deconstruct
         map-deconstruct)

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
