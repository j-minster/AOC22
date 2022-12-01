#lang racket
(require threading)

(define inp (file->lines "./input-day1-1.txt")) ; collect input

(define (partition-on lst elem)                 ; function to partition on the `elem's of a list
  (define pred (lambda (x) (not (equal? x elem))))
  (cond
    ((not (member elem lst)) (cons lst null))
    ((null? (cdr lst)) null)
    (else (cons (takef lst pred)
                (partition-on (cdr (dropf lst pred))
                              elem)))))

(define (tree-map proc tree)                    ; apply proc to all leaves in tree
  (cond ((null? tree) null)
        ((pair? tree)
         (cons
          (tree-map proc (car tree))
          (tree-map proc (cdr tree))))
        (else (proc tree))))

(~>> inp
     (partition-on _ "")                        ; partition list on ""
     (tree-map string->number)                  ; convert leaves to numbers
     (map (lambda (x) (foldl + 0 x)))           ; sum each sublist
     (apply max))                               ; find the max of these sums
