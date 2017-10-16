;; This is a set of functions
;; for numbers.
;; 1 October 2017

(define add1
 (lambda (n)
  (+ n 1)))

(define sub1
 (lambda (n)
  (- n 1)))

(define o+
 (lambda (n m)
  (cond
  ((zero? m) n)
  (else
   (add1 (o+ n (sub1 m)))))))

(define o-
 (lambda (n m)
  (cond
  ((zero? m) n)
  (else
   (sub1 (o- n (sub1 m)))))))

(define addtup
 (lambda (tup)
  (cond
   ((null? tup) 0)
   (else
    (o+ (car tup)(addtup (cdr tup)))))))

(define ox
 (lambda (n m)
  (cond
   ((zero? m) 0)
   (else
    (o+ n (ox n (sub1 m)))))))

(define tupLen
 (lambda (tup)
  (cond
   ((null? tup) 0)
   (else
    (add1 (tupLen (cdr tup)))))))

(define same_tup+
 (lambda (tup1 tup2)
  (cond
   ((and (null? tup1)(null? tup2)) (quote ()))
   (else
    (cons
     (o+ (car tup1)(car tup2))
     (gen_tup+ (cdr tup1)(cdr tup2)))))))

(define gen_tup+
 (lambda (tup1 tup2)
  (cond
   ((null? tup1) tup2)
   ((null? tup2) tup1)
   (else
    (cons
     (o+(car tup1)(car tup2))
     (gen_tup+(cdr tup1)(cdr tup2)))))))


(define o>
 (lambda (n m)
  (cond
   ((zero? n) #f)
   ((zero? m) #t)
   (else
    (o> (sub1 n)(sub1 m))))))

(define o<
 (lambda (n m)
  (cond
   ((zero? m) #f)
   ((zero? n) #t)
   (else
    (o< (sub1 n)(sub1 m))))))

(define o=
 (lambda (n m)
  (cond
   ((o< n m) #f)
   ((o> n m) #f)
   (else #t))))

(define o^
 (lambda (n m)
  (cond
   ((zero? m) 1)
   (else
    (ox n (o^ n (sub1 m)))))))


(define div
 (lambda (n m)
  (cond
   ((< n m) 0)
   (else
    (add1(div (- n m) m))))))


(define pick
 (lambda (n lat)
  (cond
   ((zero? (sub1 n)) (car lat))
   (else
    (pick (sub1 n) (cdr lat))))))

(define rempick
 (lambda (n lat)
  (cond
   ((zero? (sub1 n)) (cdr lat))
   (else 
    (cons
     (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums-lat
 (lambda (lat)
  (cond
   ((null? lat) (quote ()))
   ((number? (car lat)) (no-nums-lat (cdr lat)))
   (else
    (cons
     (car lat)(no-nums-lat (cdr lat)))))))

(define all-nums-lat
 (lambda (lat)
  (cond
   ((null? lat) (quote ()))
   ((number? (car lat))
    (cons
     (car lat)(all-nums-lat (cdr lat))))
   (else
    (all-nums-lat (cdr lat))))))

(define eqan?
 (lambda (a b)
  (cond
   ((and (number? a)(number? b))
    (= a b))
   ((or (number? a)(number? b))
    (#f))
   (else
    ((eq? a b))))))

(define occur
 (lambda (a lat)
  (cond
   ((null? lat) 0)
   ((eq? (car lat) a)
    (add1 (occur a (cdr lat))))
   (else
    (occur a (cdr lat))))))


(define one?
 (lambda (n)
  (= 1 n)))

(define rempick2
 (lambda (n lat)
  (cond
   ((one? n) (cdr lat))
   (else
    (cons
     (car lat) (rempick (sub1 n) (cdr lat)))))))


(define latsum
 (lambda (lat)
  (cond
   ((null? lat) 0)
   (else
    (o+ (car lat) (latsum (cdr lat)))))))
    
(define no_numbers
 (lambda (lat)
  (cond
   ((null? lat) (quote ()))
   ((number? (car lat)) (no_numbers (cdr lat)))
   (else
    (cons
     (car lat) (no_numbers (cdr lat)))))))
