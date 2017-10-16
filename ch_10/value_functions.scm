;; Functions from chapter 10 of
;; little schemer

(load "../again_functions.scm")

(define lookup-in-entry
 (lambda (name entry entry-f)
  (lookup-in-entry-help name
   (first entry)
   (second entry)
   entry-f)))

(define lookup-in-entry
 (lambda (name names vals entry-f)
  (cond
   ((null? names) (entry-f name))
   ((eq? (car names) name)
    (car vals))
   (else
    (lookup-in-entry
     (name (cdr names) (cdr vals) entry-f))))))

(define extend-table cons)
(define new-entry build)

(define lookup-in-table
 (lambda (name table table-f)
  (cond 
   ((null? table) (table-f name))
   (else
    (lookup-in-entry name
     (car table)
     (lambda (name)
      (lookup-in-table name
       (cdr table)
       (table-f))))))))

(define atom-to-action
 (lambda (e)
  (cond
   ((eq? #t e) *const)
   ((eq? #f e) *const)
   ((number? e) *const)
   ((eq? e (quote cons)) *const)
   ((eq? e (quote car)) *const)
   ((eq? e (quote cdr)) *const)
   ((eq? e (quote null?)) *const)
   ((eq? e (quote eq?)) *const)
   ((eq? e (quote atom?)) *const)
   ((eq? e (quote zero?)) *const)
   ((eq? e (quote add1)) *const)
   ((eq? e (quote sub1)) *const)
   ((eq? e (quote number?)) *const)
   (else *identifier))))

(define list-to-action
 (lambda (e)
  (cond
   ((atom? (car e))
    (cond
     ((eq? (car e) (quote quote)) *quote)
     ((eq? (car e) (quote lambda)) *lambda)
     ((eq? (car e) (quote cond)) *cond)
     (else *application)))
   (else *application))))

(define expression-to-action
 (lambda (e)
  (cond
   ((atom? e) (atom-to-action e))
   (else (list-to-action)))))

(define value
 (lambda (e)
  (meaning e (quote ()))))

(define meaning
 (lambda (e table)
  ((expression-to-action e) e table)))


(define *const
 (lambda (e table)
  (cond
   ((number? e) e)
   ((eq? e #t) #t)
   ((eq? e #f) #f)
   (else (build (quote primitive e))))))
   
(define text-of second)

(define *quote
 (lambda (e table)
  (text-of e)))

(define *identifier
 (lambda (e table)
  (lookup-in-table e table initial-table)))

(define initial-table
 (lambda (name)
  (car (quote ()))))

(define *lambda
 (lambda (e table)
  (build (quote non-primitive)
   (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define question-of first)
(define answer-of second)

(define else?
 (lambda (x)
  (cond
   ((atom? x)(eq? x (quote else)))
   (else #f))))

(define evcon
 (lambda (lines table)
  (cond
   ((else? (question-of (car lines)))
    (meaning (answer-of (car lines))
     table))
   ((meaning (question-of (car lines))
     table)
    (meaning (answer-of (car lines))
     table))
  (else (evcon (cdr lines) table))))) 


(define *cond
 (lambda (e table)
  (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
 (lambda (args table)
  ((null? args) (quote ()))
  (else
   (cons
    (meaning (car args) table)
    (evlis (cdr args) table)))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
 (lambda (l)
  (eq? (first l) (quote primitive))))

(define non-primitive
 (lambda (l)
  (eq? (first l) (quote non-primitive)))

(define *application
 (lambda (e table)
  (aply
   (meaning (function-of e) table)
   (evlis (arguments-of e)  table))))

(define apply
 (lambda (fun vals)
  (cond
   ((primitive? fun)
    (apply-primitive
     (second fun) vals))
   ((non-primitive? fun)
    (apply-closure
     (second fun) vals)))))

(define apply-primitive
 (lambda (name vals)
  (cond
   ((eq? name (quote cons))
    (cons (first vals) (second vals)))
   ((eq? name (quote car))
    (car (first vals)))
   ((eq? name (quote cdr))
    (cdr (first vals)))
   ((eq? name (quote null?))
    (null? (first vals)))
   ((eq? name (quote eq?))
    (eq? (first vals) (second vals)))
   ((eq? name (quote atom?))
    (atom? (first vals)))
   ((eq? name (quote zero?))
    (zero? (first vals)))
   ((eq? name (quote add1))
    (add1 (first vals)))
   ((eq? name (quote sub1))
    (sub1 (first vals)))
   ((eq? name (quote number?))
    (number? (first vals))))))

(define apply-closure
 (lambda (closure vals)
  (meaning (body-of closure)
   (extend-table
    (new-entry
     (formals-of closure)
     vals)
    (table-of closure)))))
    
