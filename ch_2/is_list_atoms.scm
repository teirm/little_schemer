;; A function to determine if a list 
;; only contains atoms


(load "../ch_1/is_atom.scm")


(define lat?
 (lambda (l)
  (cond
   ((null? l) #t)
   ((atom? (car l)) (lat? (cdr l)))
   (else #f))))

(define member?
 (lambda (a lat)
  (cond
   ((null? lat) #f)
   (else (or (eq? a (car lat))
          (member? a (cdr lat)))))))

(define not_member?
 (lambda (a lat)
  (cond
   ((null? lat) #t)
   (else (or (not (eq? a (car lat)))
          (member? a (cdr lat)))))))

;; Checks to see if list has 
;; duck
(define has_duck?
 (lambda (lat)
  (member? 'duck lat)))

;; Checks to see if list has
;; no chicken
(define no_chicken?
 (lambda (lat)
  (not (member? 'chicken lat))))

;; Checks to see if x and y are
;; not equal.

(define neq?
 (lambda (x y)
  (not (eq? x y))))
