;; Examples and exercises 
;; from chapter 7

(load "../ch_5/star_functions.scm")
(load "../ch_3/cons_functions.scm")

(define set?
 (lambda (lat)
  (cond
   ((null? lat) #t)
   ((member? (car lat) (cdr lat)) #f)
   (else
    (set? (cdr lat))))))


(define makeset
 (lambda (lat)
  (cond
   ((null? lat) (quote ()))
   ((member? (car lat) (cdr lat))
    (cons 
     (car lat)
     (makeset (multirember (car lat) (cdr lat)))))
   (else
    (cons
     (car lat)(makeset (cdr lat)))))))


(define subset?
 (lambda (s1 s2)
  (cond 
   ((null? s1) #t)
   ((member? (car s1) s2)
    (subset? (cdr s1) s2))
   (else #f))))


(define eqset?
 (lambda (s1 s2)
   (and (subset? s1 s2)(subset? s2 s1))))


(define intersect?
 (lambda (s1 s2)
  (cond
   ((null? s1) #f)
   (else
    (or (member? (car s1) s2)(intersect? (cdr s1) s2))))))


(define intersect
 (lambda (s1 s2)
  (cond
   ((null? s1) (quote ()))
   ((member? (car s1) s2)
    (cons
     (car s1)(intersect (cdr s1) s2)))
   (else
      (intersect (cdr s1) s2)))))

(define union
 (lambda (s1 s2)
  (cond
   ((null? s1) s2)
   ((not (member? (car s1) s2))
     (cons (car s1) 
      (union (cdr s1) s2)))
   (else
    (union (cdr s1) s2)))))

(define intersectall
 (lambda (lset)
  (cond 
   ((null? (cdr lset)) (car lset))
   (else
    (intersect 
     (car lset)(intersectall (cdr lset)))))))

(define ispair?
 (lambda (x)
  (cond
   ((atom? x) #f)
   ((null? x) #f)
   ((null? (cdr x)) #f)
   ((null? (cdr (cdr x))) #t)
   (else #f))))
    
(define first
 (lambda (p)
  (car p)))

(define second
 (lambda (p)
  (car (cdr p))))

(define build
 (lambda (s1 s2)
  (cons s1 (cons s2 (quote ())))))

(define third
 (lambda (t)
  (car (cdr (cdr p)))))

(define fun?
 (lambda (rel)
  (set? (firsts rel))))

(define revrel
 (lambda (rel)
  (cond
   ((null? rel) (quote ()))
   (else (cons (build 
                (second (car rel))
                (first (car rel)))
          (revrel (cdr rel)))))))

(define revpair
 (lambda (pair)
  (build (second pair)(first pair))))

(define simp_revrel
 (lambda (rel)
  (cond
   ((null? rel) (quote ()))
   (else
    (cons 
     (revpair (car rel))
     (simp_revrel (cdr rel)))))))

(define seconds
 (lambda (llat)
  (cond
   ((null? llat) (quote ()))
   (else
    (cons
     (car (cdr (car llat)))
     (seconds (cdr llat)))))))

