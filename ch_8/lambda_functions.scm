;; Functions for chapter 8
;; Lambda the Ultimate

(load "../ch_7/relations.scm")
(load "../ch_6/shadows.scm")
(load "../ch_4/number_functions.scm")

(define rember-f (lambda (test? a l) (cond ((null? l) (quote ()))
   ((test? (car l) a) (cdr l))
   (else
    (cons
     (car l) (rember-f test? a (cdr l)))))))

(define eq?-a
 (lambda (a)
  (lambda (x)
   (eq? x a))))

(define rember-f2
 (lambda (test?)
  (lambda (a l)
   (cond 
    ((null? l) (quote ()))
    ((test? (car l) a) (cdr l))
    (else
     (cons
      (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f2 eq?))

(define multiInsertL-f
 (lambda (test?)
  (lambda (n o l)
   (cond
    ((null? l) (quote ()))
    ((test? (car l) o)
     (cons n
      (cons o
       ((multiInsertL-f test?) n o (cdr l)))))
    (else
     (cons
      (car l)((multiInsertL-f test?) n o (cdr l))))))))

(define multiInsertR-f
 (lambda (test?)
  (lambda (n o l)
   (cond
    ((null? l) (quote ()))
    ((test? (car l) o)
     (cons o
      (cons n ((multiInsertR-f test?) n o (cdr l)))))
    (else
     (cons
      (car l)((multiInsertR-f test?) n o (cdr l))))))))

(define seqR
 (lambda (n o l)
  (cons n (cons o l))))

(define seqL
 (lambda (n o l)
  (cons o (cons n l)))) 

(define multiInsert-g
 (lambda (seq)
  (lambda (n o l)
   (cond
    ((null? l) (quote ()))
    ((eq? (car l) o)
     (seq n o ((multiInsert-g seq) n o (cdr l))))
    (else
     (cons
      (car l)((multiInsertR-g seq) n o (cdr l))))))))

(define seqS
 (lambda (n o l)
  (cons n l))) 

(define subst
 (lambda (n o l)
  (cond
   ((null? l) (quote ()))
   ((eq? (car l) o)
    (cons n (subst n o (cdr l))))
   (else
    (cons
     (car l)(subst n o (cdr l)))))))

(define atom-to-function
 (lambda (x)
  (cond 
   ((eq? x (quote +)) +)
   ((eq? x (quote *)) *)
   (else -))))


(define value
 (lambda (nexp)
  (cond
   ((atom? nexp) nexp)
   (else
    (atom-to-function (operator nexp)
     (value (first_sub_exp nexp))
     (value (second_sub_exp nexp)))))))


(define multimember-f
 (lambda (test?)
  (lambda (a lat)
   (cond
    ((null? lat) (quote ()))
    ((test? (car lat) a)
     ((multimember-f test?) a (cdr lat)))
    (else
     (cons
      (car lat)((multimember-f test?) a (cdr lat))))))))

(define multimember&co
 (lambda (a lat col)
  (cond
   ((null? lat)
    (col (quote ())(quote ())))
   ((eq? (car lat) a)
    (multimember&co a
     (cdr lat)
     (lambda (newlat seen)
      (col newlat
       (cons (car lat) seen)))))
   (else
    (multimember&co a
     (cdr lat)
     (lambda (newlat seen)
      (col (cons (car lat) newlat)
       seen)))))))

(define multiInsertLR
 (lambda (a oldL oldR lat)
  (cond
   ((null? lat) (quote ()))
   ((eq? (car lat) oldL)
    (cons a
     (cons oldL
      (multiInsertLR a oldL oldR (cdr lat)))))
   ((eq? (car lat) oldR)
    (cons oldR
     (cons a
      (multiInsertLR a oldL oldR (cdr lat)))))
   (else
    (cons
     (car lat) (multInsertLR a oldL oldR (cdr lat)))))))
    
(define multiInsertLR&co
 (lambda (a oldL oldR lat col)
  (cond
   ((null? lat)
    (col (quote ()) 0 0))
   ((eq? (car lat) oldL)
      (multiInsertLR a oldL oldR (cdr lat)
       (lambda (newLat L R)
        (col (cons new
              (cons oldL newLat))
         (add1 L) R))))
   ((eq? (car lat) oldR)
      (multiInsertLR a oldL oldR (cdr lat)
       (lambda (newLat L R)
        (col (cons oldR 
              (cons new newLat))
         L (add1 R)))))
   (else
    (cons
     (car lat)
     (multInsertLR a oldL oldR (cdr lat)
      (lambda (newLat L R)
       (col (cons (car lat) newLat) L R))))))))

(define even?
 (lambda (n)
  (= (* (div n 2) 2) n)))

(define evens_only*
 (lambda (l)
  (cond
   ((null? l) (quote ()))
   ((list? (car l))
    (cons
     (evens_only* (car l))
     (evens_only* (cdr l))))
   ((even? (car l))
    (cons 
     (car l)(evens_only* (cdr l))))
   (else
    (evens_only* (cdr l))))))

(define evens_only*&co
 (lambda (l col)
  (cond
   ((null? l)
    (col (quote ()) 1 0))
   ((atom? (car l))
    (cond
     ((even? (car l))
      (evens-only*&co (cdr l)
       (lambda (newl p s)
        (col (cons (car l) newl)
         (* (car l) p) s))))
     (else (evens-only*&co (cdr l)
            (lambda (newl p s)
             (col newl
              p (+ (car l) s)))))))
   (else (evens-only*&co (car l)
          (lambda (al ap as)
           (evens_only*&co (cdr l)
            (lambda (dl dp ds)
             (col (cons al dl)
              (* ap dp)
              (+ as ds))))))))))
