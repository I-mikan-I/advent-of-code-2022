#lang racket

(provide file->string)

(define (file->string path)
  (port->string (open-input-file path)))
