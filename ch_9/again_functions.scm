;; Not sure what to call these
;; but they come from chapter 9 

(load "../ch_7/relations.scm")
(load "../ch_4/number_functions.scm")

(define keep-looking
 (lambda (a n lat)
  (cond
   ((number? n) 
    (keep-looking a (pick n lat) lat))
   (else
    (eq? n a)))))

(define shift
 (lambda (p)
  (build
   (first(first p))
   (build
    (second(first p))
    (second p)))))

(define length*
 (lambda (pora)
  (cond
   ((atom? pora) 1)
   (else
    (+
     (length* (first pora))
     (length* (second pora)))))))

(define weight*
 (lambda (pora)
  (cond
   ((atom? pora) 1)
   (else
    (+
     (*
      (weight* (first pora)) 2)
     (weight* (second pora)))))))

(define shuffle
 (lambda (pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora))
    (shuffle (revpair pora)))
   (else
    (build (first pora)
     (shuffle (second pora)))))))

(define C
 (lambda (n)
  (cond
   ((one? n) 1)
   (else
    (cond
     ((even? n) (C (div n 2)))
     (else
      (C (add1 (* 3 n)))))))))

;; Into the abyss

(define eternity
 (lambda (x)
  (eternity x)))

;; length_0
(lambda (l)
 (cond
  ((null? l) 0)
  (else (add1 (eternity (cdr l))))))

;; length_1
(lambda (l)
 (cond 
  ((null? l) 0)
  (else
   (add1
    ((lambda  (l)
      (cond
       ((null? l) 0)
       (else
        (add1
         (eternity (cdr l))))))
     (cdr l))))))

;; length_2
(lambda (l)
 (cond
  ((null? l) 0)
  (else
   (add 1
    ((lambda (l)
     (cond 
      ((null? l) 0)
      (else
       (add1
        ((lambda  (l)
          (cond
           ((null? l) 0)
           (else
            (add1
             (eternity (cdr l))))))
        (cdr l))))))
     (cdr l))))))

((lambda (len)
 (lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (len (cdr l)))))))
 eternity)

((lambda (f)
 (lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add 1 (f (cdr l)))))))
 ((lambda (g)
  (lambda (l)
   (cond
    ((null? l) 0)
    (else (add1 (len (cdr l)))))))
  eternity))

((lambda (h)
  (lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add 1 (h (cdr l)))))))
 ((lambda (f)
  (lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add 1 (f (cdr l)))))))
 ((lambda (g)
  (lambda (l)
   (cond
    ((null? l) 0)
    (else (add1 (len (cdr l)))))))
  eternity)))

((lambda (mk-length)
  (mk-length eternity))
 (lambda (len)
  (lambda (l)
   (cond
    ((null? l) 0)
    (else (add1 (len (cdr l))))))))

((lambda (mk-length)
  (mk-length
   (mk-length 
    eternity)))
 (lambda (len)
  (lambda (l)
   (cond
    ((null? l) 0)
    (else (add1 (len (cdr l))))))))

((lambda (mk-length)
  (mk-length
   (mk-length
    (mk-length 
     eternity))))
 (lambda (len)
  (lambda (l)
   (cond
    ((null? l) 0)
    (else (add1 (len (cdr l))))))))

((lambda (mk-length)
  (mk-length mk-length))
 (lambda (mk-length)
  (lambda (l)
   (cond
    ((null? l) 0)
    (else (add1 (mk-length (cdr l))))))))

