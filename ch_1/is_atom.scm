;; A small function to determine if 
;; an argument is an atom.

;; 24-September-2017

(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))
