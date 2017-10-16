;; This is a set of functions
;; that presumably all operate
;; on lists of lists

(load "../ch_2/is_list_atoms.scm")


(define rember*
 (lambda (a lat)
  (cond
   ((null? lat) (quote ()))
   ((list? (car lat))
    (cons
     (rember* a (car lat))(rember* a (cdr lat))))
   ((eq? (car lat) a)
    (rember* a (cdr lat)))
   (else
    (cons
     (car lat)(rember* a (cdr lat)))))))

(define latsum*
 (lambda (lat)
  (cond
   ((null? lat) 0)
   ((list? (car lat))
    (+
     (latsum* (car lat))(latsum* (cdr lat))))
   (else
    (+
     (car lat)(latsum* (cdr lat)))))))

(define insertR*
 (lambda (n o lat)
  (cond
   ((null? lat) (quote ()))
   ((list? (car lat))
    (cons
     (insertR* n o (car lat))(insertR* n o (cdr lat))))
   ((eq? (car lat) o)
    (cons o
     (cons n (insertR* n o (cdr lat)))))
   (else
    (cons
     (car lat)(insertR* n o (cdr lat)))))))

(define occur*
 (lambda (a lat)
  (cond
   ((null? lat) 0)
   ((list? (car lat))
    (+
     (occur* a (car lat))(occur* a (cdr lat))))
   (else
    (cond
     ((eq? (car lat) a)
      (+ 1 (occur* a (cdr lat))))
     (else
      (occur* a (cdr lat))))))))

(define subst*
 (lambda (n o lat)
  (cond
   ((null? lat) (quote ()))
   ((list? (car lat))
    (cons
     (subst* n o (car lat))(subst* n o (cdr lat))))
   (else
    (cond
     ((eq? (car lat) o)
      (cons n (subst* n o (cdr lat))))
     (else
      (cons 
       (car lat)(subst* n o (cdr lat)))))))))

(define insertL*
 (lambda (n o lat)
  (cond
   ((null? lat) (quote ()))
   ((list? (car lat))
    (cons
     (insertL* n o (car lat))(insertL* n o (cdr lat))))
   (else
    (cond
     ((eq? (car lat) o)
      (cons
       (n (cons
           (o (insertL* n o (cdr lat)))))))
     (else
      (cons
       (car lat) (insertL* n o (cdr lat)))))))))

(define member?*
 (lambda (a lat)
  (cond 
   ((null? lat) #f)
   ((list? (car lat)) 
    (or 
     (member?* a (car lat))(member?* a (cdr lat))))
   (else
    (or (eq? (car lat) a) (member?* a (cdr lat)))))))
   
(define leftmost
 (lambda (lat)
  (cond
   ((null? lat) (quote ()))
   ((list? (car lat))
    (leftmost (car lat)))
   (else
    (car lat)))))

(define eqlist?
 (lambda (lat_a lat_b)
  (cond
   ((and (null? lat_a)(null? lat_b)) #t)
   ((and (null? lat_a)(not (null? lat_b))) #f)
   ((and (not (null? lat_a))(null? lat_b)) #f) 
   ((eq?(car lat_a) (car lat_b))
    (eqlist? (cdr lat_a)(cdr lat_b)))
   ((and (list? (car lat_a))(list? (car lat_b))) 
    (and
     (eqlist?
      (car lat_a)(car lat_b))
     (eqlist?
      (cdr lat_a)(cdr lat_b))))
   (else #f))))

(define equalS?
 (lambda (a b)
  (cond
   ((and (atom? a)(atom? b))
    (eq? a b))
   ((or
    (atom? a)(atom? b)) #f)
   (else
    (eqlist? a b)))))
