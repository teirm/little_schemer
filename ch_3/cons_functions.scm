;; Functions using cons
;; 30 Septemeber 2017

(define rember
 (lambda (a lat)
  (cond
   ((null? lat) (quote ()))
   ((eq? a (car lat)) (cdr lat))
   (else (cons (car lat) ;; we know the first element is not a, save it to new list
          (rember a (cdr lat))))))) ;; recur with remaining elements

(define firsts
 (lambda (lat)
  (cond
  ((null? lat) (quote ()))
  (else
   (cons
    (car (car lat))
    (firsts (cdr lat)))))))

(define insertR
 (lambda (new old lat)
  (cond
   ((null? lat) (quote ()))
   ((eq? old (car lat)) 
    (cons(car lat)(cons new (cdr lat))))
   (else
    (cons
     (car lat) (insertR new old (cdr lat)))))))

(define insertL
 (lambda (new old lat)
  (cond
   ((null? lat) (quote ()))
   ((eq? old (car lat))
    (cons new lat))
   (else
    (cons
     (car lat)(insertL new old (cdr lat)))))))

(define subst
 (lambda (new old lat)
  (cond
   ((null? lat) (quote ()))
   ((eq? old (car lat))
    (cons new (cdr lat)))
   (else
    (cons
     (car lat)(subst new old (cdr lat)))))))

(define subst2
 (lambda (new old1 old2 lat)
  (cond
   ((null? lat) (quote ()))
   ((or(eq? old1 (car lat))(eq? old2 (car lat)))
    (cons new (cdr lat)))
   (else
    (cons
     (car lat)(subst2 new old1 old2 (cdr lat)))))))

(define multirember
 (lambda (a lat)
  (cond
   ((null? lat) (quote ()))
   ((eq? (car lat) a) (multirember a (cdr lat)))
   (else
    (cons (car lat)
     (multirember a (cdr lat)))))))

    
(define multiInsertR
 (lambda (new old lat)
  (cond
   ((null? lat) (quote ()))
   ((eq? (car lat) old)
    (cons old (cons new (multiInsertR new old (cdr lat)))))
   (else
    (cons
     (car lat)(multiInsertR new old (cdr lat)))))))
  

(define multiInsertL
 (lambda (new old lat)
  (cond
   ((null? lat) (quote ()))
   ((eq? (car lat) old)
    (cons new (cons (car lat) (multiInsertL new old (cdr lat)))))
   (else
    (cons
     (car lat)(multiInsertL new old (cdr lat)))))))

(define multiSubst
 (lambda (new old lat)
  (cond
   ((null? lat) (quote ()))
   ((eq? (car lat) old)
    (cons new (multiSubst new old (cdr lat))))
   (else
    (cons
     (car lat)(multiSubst new old (cdr lat)))))))




