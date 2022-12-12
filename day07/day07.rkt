#lang racket
(require "../utils/text.rkt")
(define input (file->string "day07/input.txt"))

(struct directory (name parent subdirs file-sum) #:transparent #:mutable)

(define (ls output-lst subdir)
  (for/fold ([dire subdir]) ([l output-lst])
    (define l2 (string->list l))
    (match l2
      [(list #\d #\i #\r #\  name ...)
       (let ([child (directory name dire (make-hash) 0)])
         (hash-set! (directory-subdirs dire) (list->string name) child)
         dire)]
      [(list num ... #\  _ ...)
       (set-directory-file-sum! dire
                                (+ (directory-file-sum dire) (string->number (list->string num))))
       dire])))

(define (cd dir-name parent)
  (if (equal? dir-name "..")
      (directory-parent parent)
      (hash-ref (directory-subdirs parent) dir-name)))

(define (read-root str)
  (define commands (string-split str "$ "))
  (define root (directory "/" (void) (make-hash) 0))
  (let rec ([command (string-split (car commands) "\n")]
            [rest (cdr commands)]
            [current-dir (directory "root" (void) (make-hash (list (cons "/" root))) 0)])
    (define new-dir
      (match (string->list (car command))
        [(list #\l #\s rest ...) (ls (cdr command) current-dir)]
        [(list #\c #\d #\  name ...) (cd (list->string name) current-dir)]))
    (if (empty? rest) root (rec (string-split (car rest) "\n") (cdr rest) new-dir))))

(define (size dir)
  (define subdirs (directory-subdirs dir))
  (if (hash-empty? subdirs)
      (directory-file-sum dir)
      (+ (directory-file-sum dir) (foldl + 0 (map size (hash-values subdirs))))))

(define (accum dir)
  (let rec ([current dir] [sum 0])
    (define sum2 (+ 0 (if (<= (size current) 100000) (size current) 0)))
    (if (hash-empty? (directory-subdirs current))
        (+ sum sum2)
        (foldl + sum2 (map (lambda (d) (rec d sum)) (hash-values (directory-subdirs current)))))))

(define (dir->list dir)
  (cons dir (foldl append '() (map dir->list (hash-values (directory-subdirs dir))))))

(define (find-smallest dir limit)
  (define lst (dir->list dir))
  (define sorted (sort (map size lst) <))
  (car (filter (lambda (s) (>= s limit)) sorted)))

(define root (read-root input))
(accum root)
(find-smallest root (- 30000000 (- 70000000 (size root))))