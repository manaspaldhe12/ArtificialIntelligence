  (define (check-new current-placements new_placement new_entry)
    (cond ((and (across-entry? new_entry) (not (equal? (across-entry-length new_entry) (string-length new_placement)))) #f)
    ((and (down-entry? new_entry) (not (equal? (down-entry-length new_entry) (string-length new_placement)))) #f)
    ((null? current-placements) #t)
    ((not (consistent-entries? (second (first current-placements)) new_entry (first (first current-placements)) new_placement )) #f)
    (else (check-new (rest current-placements) new_placement new_entry))))

    (define (get-next-position entry words)
    (list entry (a-member-of words)))

    (define (generate-and-test current-placements remaining-positions words)
    (cond ((null? remaining-positions)
    (if (check-new (rest current-placements) (first (first current-placements)) (second (first current-placements)))
    current-placements
    (fail)))
    ((null? current-placements) (generate-and-test (list (list (a-member-of words) (first remaining-positions))) (rest remaining-positions) words))
    ((check-new (rest current-placements) (first (first current-placements)) (second (first current-placements))) (generate-and-test (cons (list (a-member-of words) (first remaining-positions)) current-placements) (rest remaining-positions) words))
    (else (fail))))

    (define (solve-crossword-puzzle-by-backtracking entries words)
    (place-sol (generate-and-test '() entries words)))

    (define (place-sol sol)
    (if (null? sol)
    #f
    (list (fill-entry (second (first sol)) (first (first sol))) (place-sol (rest sol)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  After Backtracking   ;;;;;;;;;;;;;;;;;;;;;



    (define (assert-unary-constraint-gfc! constraint x)
    (restrict-domain!
    x (remove-if-not (lambda (xe) (constraint xe))
    (domain-variable-domain x))))

    (define (assert-binary-constraint-gfc! constraint x y)
    (attach-after-demon!
    (lambda ()
    (when (bound? x)
    (restrict-domain!
    y (remove-if-not (lambda (ye) (constraint (binding x) ye))
    (domain-variable-domain y)))))
    x)
    (attach-after-demon!
    (lambda ()
    (when (bound? y)
    (restrict-domain!
    x (remove-if-not (lambda (xe) (constraint xe (binding y)))
    (domain-variable-domain x)))))
    y))

    (define (assert-unary-constraint-ac! constraint x)
    (restrict-domain!
    x (remove-if-not (lambda (xe) (constraint xe))
    (domain-variable-domain x))))

    (define (assert-binary-constraint-ac! constraint x y)
    (attach-after-demon!
    (lambda ()
    (restrict-domain!
    y (remove-if-not (lambda (ye)
    (some (lambda (xe) (constraint xe ye))
    (domain-variable-domain x)))
    (domain-variable-domain y))))
    x)
    (attach-after-demon!
    (lambda ()
    (restrict-domain!
    x (remove-if-not (lambda (xe)
    (some (lambda (ye) (constraint xe ye))
    (domain-variable-domain y)))
    (domain-variable-domain x))))
    y))

    (define (generate-domain entry words)
    (if (null? words)
    '()
    (cons (list entry (first words)) (generate-domain entry (rest words)))))


    (define (generate-domain-variables entries words)
    (if (null? entries)
    '()
    (cons (create-domain-variable words) (generate-domain-variables (rest entries) words))))


    (define (attach-place-word-daemon entry domain-variable)
    (attach-after-demon!
    (lambda ()
    (when (bound? domain-variable) (fill-entry entry (binding domain-variable))))
    domain-variable))

    (define (attach-place-word-daemons entries list-domain-variable)
    (if (null? list-domain-variable)
    '()
    (cons (attach-place-word-daemon (first entries) (first list-domain-variable)) (attach-place-word-daemons (rest entries) (rest list-domain-variable)))))

    (define (constraint-1-d pair)
    (cond ((and (across-entry? (first pair)) (equal? (across-entry-length (first pair)) (string-length (second pair)))) #t)
    ((and (down-entry? (first pair)) (equal? (down-entry-length (first pair)) (string-length (second pair)))) #t)
    (else #f)))

    (define (constraint-2-d pair1 pair2)
    (consistent-entries? (first pair1) (first pair2) (second pair1) (second pair2)))

    (define (create-var1-varlist-constraint entry var1 entries var-list)
    (if (null? var-list)
    (assert-constraint! constraint-1-d (list entry var1))
    (cons (assert-constraint! constraint-2-d (list (list entry var1) (list (first entries) (first var-list))) (create-var1-varlist-constraint entry var1 (rest entries) (rest var-list))))))

    (define (create-var-constraints entries domain-vars)
    (if (null? domain-vars)
    '()
    (cons (create-var1-varlist-constraint (first entries) (first domain-vars) (rest entries) (rest domain-vars)) (create-var-constraints (rest entries) (rest domain-vars)))))

    ;;(define (solve-crossword-puzzle-by-constraints entries words)
    ;;(let (( domain-vars (generate-domain-variables entries words)))
    ;;(attach-place-word-daemons entries domain-vars)
    ;;(create-var-constraints entries domain-vars)
    ;;(csp-solution domain-vars first)))


    (define (get-ith-entry entries i)
    (cond ((null? entries) '())
    ((equal? i 0) (first entries))
    (else (get-ith-entry (rest entries) (- i 1)))))



    ;;
    (define (solve-crossword-puzzle-by-constraints entries words)
    (let ((fills (make-vector (length entries))))
    (for-each-n
    (lambda (i)
    (let ((domain-variable (create-domain-variable words)))
    (vector-set! fills i domain-variable)
    (attach-after-demon!
    (lambda ()
    (when (bound? domain-variable)
    (fill-entry (get-ith-entry entries i) (binding domain-variable))))
    domain-variable)))
    (length entries))
    (for-each-n
    (lambda (i)
    (assert-constraint!
    (lambda (fill)
    (cond ((and (across-entry? (get-ith-entry entries i) ) (equal? (across-entry-length (get-ith-entry entries i)) (string-length fill))) #t)
    ((and (down-entry? (get-ith-entry entries i)) (equal? (down-entry-length (get-ith-entry entries i)) (string-length fill))) #t)
    (else #f)))
    (list (vector-ref fills i))))
    (length entries))
    (for-each-n
    (lambda (i)
    (for-each-n
    (lambda (j)
    (when (> j i)
    (assert-constraint!
    (lambda (fill1 fill2) (consistent-entries? (get-ith-entry entries i) (get-ith-entry entries j) fill1 fill2))
    (list (vector-ref fills i) (vector-ref fills j)))))
    (length entries)))
    (length entries))
    (csp-solution (vector->list fills) first)))





;;Program FAILED test suite

;;You should only assert constraints between entries that intersect.
;;Grade: 90/100
