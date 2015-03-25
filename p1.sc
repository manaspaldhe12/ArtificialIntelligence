    (define (remove-element l x)
    (cond ((null? l) l)
    ((= x (first l)) (remove-element (rest l) x))
    (else (cons (first l) (remove-element (rest l) x)))))

    (define (member-exists l x)
    (cond ((null? l) #f)
    ((= x (first l)) #t)
    (else (member-exists (rest l) x))))

    (define (remove-duplicates-from-list l)
    (cond ((null? l) l)
    ((member-exists (rest l) (first l)) (remove-duplicates-from-list (rest l)))
    (else (cons (first l) (remove-duplicates-from-list (rest l))))))

    (define (append l1 l2)
    (if (null? l1)
    l2
    (cons (first l1) (append (rest l1) l2))))

    (define (set-union A B)
    (remove-duplicates-from-list (append A B)))

    (define (set-intersection A B)
    (cond ((null? A) A)
    ((null? B) B)
    ((member-exists B (first A)) (remove-duplicates-from-list (cons (first A) (set-intersection (rest A) B))))
    (else (remove-duplicates-from-list(set-intersection (rest A) B)))))

    (define (set-minus A B)
    (if (null? B)
    (remove-duplicates-from-list A)
    (set-minus (remove-element A (first B)) (remove-element B (first B)))))

;;Program PASSED test suite

;;Grade: 100/100
