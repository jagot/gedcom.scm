(import (except (rnrs) define-record-type)
        (srfi :1)
        (srfi :9))
(import (ice-9 peg))
(use-modules (ice-9 pretty-print))

(include "stack.scm")

(define-peg-string-patterns
  "level <-- [0-9]+
tag <-- [A-Z]+
id <-- [0-9]+
ident <-- '@' [A-Z]+ id '@'
contents <-- (! NL .)*
line <- level cSP (ident cSP)? tag (cSP ident)? (cSP contents)? NL*
gedcom-data <- line* !.
cSP < [ \t]*
NL < [\r\n]+")

(define (parse-id id)
  (list (car id) (string->number (cadr id))))

(define (parse-line data)
  (let ((level (string->number (car (cdr (car data))))))
    (cons level
          (map (lambda (d)
                 (if (and (pair? d)
                          (eq? (car d) 'ident))
                     (parse-id (caddr d))
                     d))
               (cdr data)))))

(define (parse-nodes nodes)
  (define stack (make-stack))
  (display "Parsing nodes\n")
  (let ((count (length nodes))
        (cur 1))
    (define (unwind-stack! level)
      (let* ((top (stack 'pop!))
             (next-to-top (stack 'pop!))
             (cur-level (car (car next-to-top)))
             (last-element (last next-to-top))
             (pre (drop-right next-to-top 1))
             (new-last-element (append last-element `((children ,top)))))
        (stack 'push! (append! pre `(,new-last-element)))
        (if (not (eq? level cur-level))
            (unwind-stack! level))))

    (do ((remaining nodes (cdr remaining)))
        ((null? remaining) '())
      (format #t "\r~A / ~A" cur count)
      (set! cur (+ cur 1))
      (let* ((node (car remaining))
             (level (car node)))
        (if (stack 'empty?)
            (stack 'push! `(,node))
            (let* ((top (stack 'top))
                   (top-level (car (car top))))
              (cond
               ((eq? level top-level)
                (stack 'push! (append! (stack 'pop!) `(,node))))
               ((eq? level (+ top-level 1))
                (stack 'push! `(,node)))
               ((and (>= level 0)
                     (< level top-level))
                (and (unwind-stack! level)
                     (stack 'push! (append! (stack 'pop!) `(,node)))))
               (else
                (error 'parse-nodes
                       (format #t "Invalid level jump from ~A to ~A" top-level level)
                       'level)))))))
    (newline)
    (if (> (stack 'size) 1)
        (unwind-stack! 0))
    (stack 'top)))

(define (map-progress fun lst)
  (let* ((count (length lst))
         (cur 1)
         (data
          (map (lambda (e)
                 (format #t "\r~A / ~A" cur count)
                 (set! cur (+ cur 1))
                 (fun e))
               lst)))
    (newline)
    data))

(define (parse-gedcom data)
  (display "Parsing lines\n")
  (parse-nodes (map-progress parse-line
                    (peg:tree (match-pattern gedcom-data data)))))

(define (parse-gedcom-file filename)
  (call-with-input-file filename
    (lambda (port)
      (parse-gedcom (get-string-all port)))))
