(define (check-i-j coordinate1 coordinate2)
    (cond ((equal? coordinate1 coordinate2) #f)
    ((equal? (first coordinate1) (first coordinate2)) #f)
    ((equal? (second coordinate1) (second coordinate2)) #f)
    ((equal? (- (first coordinate1) (first coordinate2)) (- (second coordinate1) (second coordinate2))) #f)
    ((equal? (- (first coordinate2) (first coordinate1)) (- (second coordinate1) (second coordinate2))) #f)
    (else #t)))

    (define (check-new positions new_pos)
    (cond ((null? positions) #t)
    ((not (check-i-j (first positions) new_pos)) #f)
    (else (check-new (rest positions) new_pos))))

    (define (get-next-position row n)
    (list row (an-integer-between 0 (- n 1))))

    (define (generate-and-test row current-positions n)
    (cond ((null? current-positions) (generate-and-test 0 (list (get-next-position 0 n)) n ))
    ((equal? row (- n 1))
    (if (check-new (rest current-positions) (first current-positions))
    current-positions
    (fail)))
    ((check-new (rest current-positions) (first current-positions)) (generate-and-test (+ 1 row) (cons (get-next-position (+ 1 row) n) current-positions) n))
    (else (fail))))

    (define (place-n-queens-by-backtracking n)
    (place-sol (generate-and-test 0 '() n)))

    (define (place-sol sol)
    (if (null? sol)
    #f
    (list (place-queen (first (first sol)) (second (first sol))) (place-sol (rest sol)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Problem 2 a) GFC   ;;;;;;;;;;;;;;;;;;;;;

    ;; Built in functions
    ;; create-domain-variable domain
    ;; attach-before-demon! demon x
    ;; attach-after-demon! demon x
    ;; restrict-domain! x domain
    ;; bound? x
    ;; binding x
    ;; csp-solution domain-variables select
    ;; assert-unary-constraint-efd! constraint x
    ;; assert-binary-constraint-efd! constraint x y
    ;; assert-ternary-constraint-efd! constraint x y z
    ;; assert-unary-constraint-fc! constraint x
    ;; assert-binary-constraint-fc! constraint x y
    ;; assert-ternary-constraint-fc! constraint x y z
    ;; assert-unary-constraint-vp! constraint x
    ;; assert-binary-constraint-vp! constraint x y
    ;; assert-ternary-constraint-vp! constraint x y z
    ;; assert-constraint! constraint domain-variables
    ;; chessboard-square
    ;; *n*
    ;; *chessboard*
    ;; redraw-chessboard-square chessboard-square
    ;; place-queen i j
    ;; make-chessboard


    ;; domain-variable-domain x

    (define (get-possible-unary constraint x original_domain)
    (cond ((null? original_domain) '())
    ((constraint (first original_domain)) (cons (first original_domain) (get-possible-unary constraint x (rest original_domain))))
    (else (get-possible-unary constraint x (rest original_domain)))))

    (define (assert-unary-constraint-gfc! constraint x)
    (restrict-domain! x (get-possible-unary constraint x (domain-variable-domain x))))

    (define (assert-unary-constraint-ac! constraint x)
    (restrict-domain! x (get-possible-unary constraint x (domain-variable-domain x))))

    (define (get-possible-binary constraint x original_x_domain y_value)
    (cond ((null? original_x_domain) '())
    ((constraint (first original_x_domain) y_value) (cons (first original_x_domain) (get-possible-binary constraint x (rest original_x_domain) y_value)))
    (else (get-possible-binary constraint x (rest original_x_domain) y_value))))

    (define (is-true-for-some constraint x_value y_values)
    (cond ((null? y_values) #f)
    ((constraint x_value (first y_values)) #t)
    (else (is-true-for-some constraint x_value (rest y_values)))))

    (define (get-possible-binary-ac constraint x original_x_domain y_values)
    (cond ((null? y_values) '())
    ((null? original_x_domain) '())
    ((is-true-for-some constraint (first original_x_domain) y_values) (cons (first original_x_domain) (get-possible-binary-ac constraint x (rest original_x_domain) y_values)))
    (else (get-possible-binary-ac constraint x (rest original_x_domain) y_values))))

    (define (assert-binary-constraint-gfc! constraint x y)
    (for-each
    (lambda (v)
    (attach-after-demon!
    (lambda ()
    (when (bound? x)
    (let ((y_values (get-possible-binary constraint y (domain-variable-domain y) (binding x))))
    (if (null? y_values)
    (fail)
    (restrict-domain! y y_values))))
    (when (bound? y)
    (let ((x_values (get-possible-binary constraint x (domain-variable-domain x) (binding y))))
    (if (null? x_values)
    (fail)
    (restrict-domain! x x_values)))))
    v))
    (list x y)))

    (define (assert-binary-constraint-ac! constraint x y)
    (for-each
    (lambda (v)
    (attach-after-demon!
    (lambda ()
    (let ((y_values (get-possible-binary-ac constraint y (domain-variable-domain y) (domain-variable-domain x))))
    (if (null? y_values)
    (fail)
    (restrict-domain! y y_values)))
    (let ((x_values (get-possible-binary-ac constraint x (domain-variable-domain x) (domain-variable-domain y))))
    (if (null? x_values)
    (fail)
    (restrict-domain! x x_values))))
    v))
    (list x y)))

    (define (generate-list row start end)
    (cond ((equal? start end) (list (list row start)))
    ((> start end) '())
    (else (cons (list row start) (generate-list row (+ 1 start) end)))))

    (define (generate-domain row n)
    (generate-list row 0 (- n 1)))

    (define (generate-row-domain-variable start-row n)
    (cond ((>= start-row n) '())
    ((< start-row 0) '())
    (else (cons (create-domain-variable (generate-domain start-row n)) (generate-row-domain-variable (+ 1 start-row) n)))))

    (define (generate-n-domain-variables n)
    (generate-row-domain-variable 0 n))

    (define (constraint-1-d coordinate)
    (cond ((and (>= (second coordinate) 0) (>= (first coordinate) 0)) #t)
    (else #f)))

    ;; (check-i-j coordinate1 coordinate2)
    (define (constraint-2-d coordinate1 coordinate2)
    (check-i-j coordinate1 coordinate2))

    (define (create-var1-varlist-constraint var1 var-list)
    (if (null? var-list)
    (assert-constraint! constraint-1-d (list var1))
    (cons (assert-constraint! constraint-2-d (list var1 (first var-list))) (create-var1-varlist-constraint var1 (rest var-list)))))

    (define (create-var-constraints domain-vars)
    (if (null? domain-vars)
    '()
    (cons (create-var1-varlist-constraint (first domain-vars) (rest domain-vars)) (create-var-constraints (rest domain-vars)))))

    (define (attach-place-queen-daemon domain-variable)
    (attach-after-demon!
    (lambda ()
    (when (bound? domain-variable) (place-queen (first (binding domain-variable)) (second (binding domain-variable)))))
    domain-variable))

    (define (attach-place-queen-daemons list-domain-variable)
    (if (null? list-domain-variable)
    '()
    (cons (attach-place-queen-daemon (first list-domain-variable)) (attach-place-queen-daemons (rest list-domain-variable)))))

    (define (place-n-queens-by-constraints n)
    (let (( domain-vars (generate-n-domain-variables n)))
    (attach-place-queen-daemons domain-vars)
    (create-var-constraints domain-vars)
    (csp-solution domain-vars first)))





;;Program PASSED test suite

;;Grade: 100/100
