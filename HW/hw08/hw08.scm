(define (ascending? s) 
    (cond ((null? s)  #t)
          ((null? (cdr s)) #t)
          (else 
                (and (<= (car s) (car (cdr s))) 
                     (ascending? (cdr s))))))

(define (my-filter pred s) 
    (cond ((null? s) nil)
          ((pred (car s)) (cons (car s) (my-filter pred (cdr s))))
          (else (my-filter pred (cdr s)))))

(define (interleave lst1 lst2) 
    (if (null? lst1) 
        lst2
        (cons (car lst1) (interleave lst2 (cdr lst1)))))

(define (no-repeats s) 
    (if (null? s) 
        nil
        (cons (car s) (no-repeats (filter (lambda (i) (not (= i (car s)))) (cdr s))))))
