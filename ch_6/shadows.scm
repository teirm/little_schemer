;; Functions and examples from
;; chapter 6.

(load "../ch_5/star_functions.scm")

(define numbered?
 (lambda (aexp)
  (cond
   ((atom? aexp) (number? aexp)
    ((eq? (car (cdr (aexp)) (quote +)))
    (and
     (numbered? (car aexp))(numbered? (car (cdr (cdr (aexp)))))))
   ((eq? (car (cdr (aexp)) (quote *)))
    (and
     (numbered? (car aexp))(numbered? (car (cdr (cdr (aexp)))))))
   (else
    (and
     (numbered? (car aexp))(numbered? (car (cdr (cdr (aexp)))))))))))

(define numbered_simp?
 (lambda (aexp)
  (cond
   ((atom? aexp) (number? aexp))
   (else
    (and
     (numbered? (car aexp))(numbered? (car (cdr (cdr (aexp))))))))))

(define value?
 (lambda (aexp)
  (cond
   ((atom? aexp) aexp)
   ((eq? (car (cdr aexp)) (quote +))
    (+
     (value? (car aexp))(value? (car (cdr (cdr aexp))))))
   ((eq? (car (cdr aexp)) (quote *))
    (*
     (value? (car aexp))(value? (car (cdr (cdr aexp))))))
   (else
    (-
     (value? (car aexp))(value? (car (cdr (cdr aexp)))))))))

(define first_sub_exp
 (lambda (aexp)
  (car (cdr (aexp)))))

(define second_sub_exp
 (lambda (aexp)
  (cdr (cdr (cdr (aexp))))))

(define operator
 (lambda (aexp)
  (car aexp)))

(define sero?
 (lambda (n)
  (null? n)))

(define edd1
 (lambda (n)
  (cons (quote ()) n)))

(define zub1
 (lambda (n)
  (cdr n)))

(define zop+
 (lambda (a n)
  (cond
   ((sero? a) n)
   (else
    (edd1
     (zop+ (zub1 a) n))))))

(define lmzp
 (lambda (m n)
  (cond
   ((sero? m) n)
   (else
    (zop+ n
     (lmzp (zub1 m) n))))))

