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

    ;; is the expression a costant, that is it a boolean, number or a symbol
    (define (expression-constant? con)
    (or (boolean? con)
    (number? con)
    (symbol? con)))

    ;; is the input is expression variable
    (define (expression-variable? var)
    (member var '(p1 p2 p3 p4 p5 a b c d e f e1 e2 e3 e4 e5 e6 phi ph1 phi2 phi3 phi4 phi5 phi6)))

    ;; is the input a pattern variable?
    (define (pattern-list-variable? pattern)
    (memq pattern '(... e.. e1.. e2.. e3.. e4.. e5.. j1... j2... j3... m1... m2... m3... v...)))

    ;; if two bindings are inconsistent //bindings is a list of binding
    (define (inconsistent? bindings1 bindings2)
    (reduce
    (lambda (p q) (or p q))
    (map (lambda (binding1)
    (reduce
    (lambda (p q) (or p q))
    (map (lambda (binding2)
    (and (eq? (first binding1) (first binding2))
    (not (equal? (second binding1) (second binding2)))))
    bindings2)
    #f))
    bindings1)
    #f))

    ;; combine result from two bindings -- returns null list if they are inconsistent, else merges them into one bingings
    (define (combine result1 result2)
    (if (or (eq? result1 #f) (eq? result2 #f) (inconsistent? result1 result2))
    #f
    (append result1 result2)))

    ;; instantiate
    (define (instantiate p bindings)
    (define (instantiate-with-bindings p) (instantiate p bindings))
    (cond ((symbol? p) (lookup p bindings))
    ((expression-constant? p) p)
    ((list? p) (map instantiate-with-bindings p))
    (else (panic "Invalid pattern"))))

    (define (find-first-matching-rule e rules)
    (if (null? rules)
    #f
    (if (eq? (match (first (first rules)) e) #f)
    (find-first-matching-rule e (rest rules))
    (first rules))))

    ;; lookup a variable
    (define (lookup variable bindings)
    (cond ((null? bindings) (panic "Unbound variable"))
    ((eq? (first (first bindings)) variable) (second (first bindings)))
    (else (lookup variable (rest bindings)))))

    (define (match p e)
    (cond ((symbol? p) (list (list p e)))
    ((expression-constant? p) (if (equal? p e) '() #f))
    ((list? p)
    (if (and (list? e) (= (length p) (length e)))
    (map-reduce combine '() match p e)
    #f))
    (else (panic "Invalid pattern"))))

    (define (contains_dots? e)
    (cond ((null? e) 0)
    ((equal? (first e) '...) #t)
    (else (contains_dots? (rest e)))))

    (define (append-dot-bindings bindings1 bindings2)
    (cond ((equal? bindings1 #f) #f)
    ((equal? bindings2 #f) #f)
    ((inconsistent? bindings1 bindings2) #f)
    (else (append bindings1 bindings2)))
    )

    (define (merge-into-all elem input-list)
    (cond
    ((null? input-list) '())
    (else (cons (cons elem (first input-list)) (merge-into-all elem (rest input-list)))))
    )

    (define (merge-into-all-splits elem split-list)
    (cond
    ((null? split-list) '())
    (else (cons (cons (cons elem (first (first split-list))) (rest (first split-list))) (merge-into-all-splits elem (rest split-list)))))
    )


    (define (generate-all-splits e)
    (cond ((null? e) (list '() '()))
    ((null? (rest e)) (list (list (list (first e)) '())))
    (else  (append (list (list (list (first e)) (rest e))) (merge-into-all-splits (first e) (generate-all-splits (rest e))))))
    )

    ;;(define (generate-dot-bindings p e)
    ;;  (cond
    ;;          ((expression-variable? p) (list (list (list p e))))
    ;;          ((expression-constant? p) (if (equal? p e) '() #f))
    ;;          (else (if (contains-dots? p)
    ;;                  (if (equal? (first p) '...)
    ;;                          (
    ;;                                  (merge-into-all '... (generate-all-binding-of-dot e)
    ;;
    ;;                                  )
    ;;                          (cross-dot-bindings (generate-dot-bindings (first p) (first e) (generate-dot-bindings (rest p) (rest e))))
    ;;                  (if (and (list? e) (= (length p) (length e)))
    ;;                          (cross-dot-bindings (generate-dot-bindings (first p) (first e)) (generate-dot-bindings (rest p) (rest e)))
    ;;                          #f)))))
    ;;

    ;;(define (match-dotted-pattern p e)
    ;;
    ;;)


    ;;(define (match-pattern p e)
    ;;(cond ((expression-variable? p) (list (list p e)))
    ;;((expression-constant? p) (if (equal? p e) '() #f))
    ;;((list? p)
    ;;  (if (contains_dots? p)
    ;;
    ;;          (cond (and (list? e) (= (length p) (length e))) (map-reduce combine '() match p e))
    ;;(else (panic "Invalid pattern"))))


    (define (rewrite e rules)
    (define (rewrite-with-rules e) (rewrite e rules))
    (let ((e (if (list? e) (map rewrite-with-rules e) e)))
    (let ((rule (find-first-matching-rule e rules)))
    (if (eq? rule #f)
    e
    (rewrite (instantiate (third rule) (match (first rule) e)) rules)))))

    (define (and-procedure a b)
    (and a b))

    (define (or-procedure a b)
    (or a b))

    (define (evaluate e bindings)
    (define (evaluate-given-binds e) (evaluate e bindings))
    (cond ((symbol? e)  (lookup e bindings))
    ((expression-constant? e) e)
    ((list? e)
    (case (first e)
    ((and) (map-reduce and-procedure #t evaluate-given-binds (rest e)))
    ((or)  (map-reduce or-procedure #f evaluate-given-binds (rest e)))
    ((not) (not (evaluate (second e) bindings)))
    (else "unknown operator")))
    (else "panic invalid expression")))

    (define (evaluateable-without-bindings e)
    (cond ((symbol? e)  #f)
    ((expression-constant? e) #t)
    ((list? e)
    (case (first e)
    ((and) (map-reduce and-procedure #t evaluateable-without-bindings (rest e)))
    ((or)  (map-reduce and-procedure #t evaluateable-without-bindings (rest e)))
    ((not) (map-reduce and-procedure #t evaluateable-without-bindings (rest e)))
    (else "unknown operator")))
    (else "panic invalid expression")))

    ;; is a given variable bounded in given bindings
    (define (is_bounded? bindings variable)
    (cond ((null? bindings) #f)
    ((eq? (first (first bindings)) variable) #t)
    (else (is_bounded? (rest bindings) variable))))

    ;; removes the redundant binds from a bindings
    (define (remove-bindings-redundancy bindings)
    (cond
    ((null? bindings) bindings)
    ((is_bounded? (rest bindings) (first (first bindings))) (remove-bindings-redundancy (rest bindings)))
    (else (cons (first bindings) (remove-bindings-redundancy (rest bindings))))))

    ;;Removes the redundant binds from all bindings in a list of bindings
    (define (remove-redundancy list_of_bindings)
    (cond
    ((null? list_of_bindings) list_of_bindings)
    (else (cons (remove-bindings-redundancy (first list_of_bindings)) (remove-redundancy (rest list_of_bindings))))))

    ;; Updates a bindings list when new bindings is seen
    (define (update_binding_list binding_list new_bind)
    (cond ((null? binding_list) (list new_bind))
    ((null? (rest binding_list)) (list (append new_bind (first binding_list))))
    (else (cons (append new_bind (first binding_list))  (update_binding_list (rest binding_list) new_bind)))))

    ;; takes the cross product of two list of possible bindings to give a new list of possible bindings. Bindings may be inconsistent and redundant
    (define (cross-product list1 list2)
    (cond
    ((null? list1) list2)
    ((null? list2) list1)
    ((null? (rest list1)) (update_binding_list list2  (first list1)))
    ((null? (rest list2)) (update_binding_list list1  (first list2)))
    (else (append (update_binding_list list1  (first list2)) (cross-product  list1 (rest list2))))))

    ;; updates the list of possible bindings when a new variable is seen
    (define (create-binding-list current_list new_variable)
    (cond       ((null? current_list) (list (list (list new_variable #t)) (list (list new_variable #f))))
    ((is_bounded? (first current_list) new_variable) current_list)
    (else (cross-product (list (list (list new_variable #t)) (list (list new_variable #f))) current_list ))))

    ;; generates the list of all possible bindings with possible redundancy. IS initially called with empty current_bindings
    (define (get-all-possible-bindings-with-duplicates e current_bindings)
    (cond ((symbol? e)
    (if (and (not (null? current_bindings)) (is_bounded? (first current_bindings) e))
    (current_bindings)
    (create-binding-list current_bindings e)))
    ((expression-constant? e) current_bindings)
    ((list? e)
    (if (null? (rest e))
    (get-all-possible-bindings-with-duplicates (first e) current_bindings)
    (case (first e)
    ((and) (get-all-possible-bindings-with-duplicates (rest e) current_bindings))
    ((or) (get-all-possible-bindings-with-duplicates (rest e) current_bindings))
    ((not) (get-all-possible-bindings-with-duplicates  (rest e) current_bindings))
    (else (cross-product (get-all-possible-bindings-with-duplicates  (first e) current_bindings) (get-all-possible-bindings-with-duplicates  (rest e) current_bindings)))))) ))

    ;; generates the list of all possible bindings given the expression e
    (define (get-all-possible-bindings e)
    (remove-redundancy (remove-inconsistent (get-all-possible-bindings-with-duplicates e '()))))

    ;; removes the inconsistent bindings from the list of possible bindings
    (define (remove-inconsistent list)
    (if (null? list)
    list
    (if (inconsistent? (first list) (first list))
    (remove-inconsistent (rest list))
    (cons (first list) (remove-inconsistent (rest list))))))

    ;; create truth table of an expression e when the list of possible bindings is given
    (define (create-truth-table e possible-bindings)
    (if (null? possible-bindings)
    (if (evaluateable-without-bindings e) (list '((p1 #t)) (evaluate e '())) '())
    (cons (list (first possible-bindings) (evaluate e (first possible-bindings))) (create-truth-table e (rest possible-bindings)))))

    ;; create truth table of a formula
    (define (truth-table phi)
    (create-truth-table phi (get-all-possible-bindings phi)))

    (define (truth-tables-match? phi1 phi2)
    (if (equal? (create-truth-table phi1 (get-all-possible-bindings phi1)) (create-truth-table phi2 (get-all-possible-bindings phi1)))
    #t
    #f))

    (define (propositions-in formula)
    (cond ((symbol? formula) (list formula))
    ((boolean? formula) '())
    ((and (list? formula) (not (null? formula)))
    (case (first formula)
    ((not) (if (= (length formula) 2)
    (propositions-in (second formula))
    (panic "Unrecognized formula")))
    ((and) (reduce unionq (map propositions-in (rest formula)) '()))
    ((or) (reduce unionq (map propositions-in (rest formula)) '()))
    (else (panic "Unrecognized formula"))))
    (else (panic "Unrecognized formula"))))



    (define (rewrite-rules)
    '(((not #t) ~> #f)
    ((not #f) ~> #t)
    ((not (not e)) ~> e)
    ((and) ~> #t)
    ((and e) ~> e)
    ((or) ~> #f)
    ((or e) ~> e)))


    (define (rewrite-rule-1 phi)
    (if (equal? phi '(not #t))
    #f
    phi))

    (define (rewrite-rule-2 phi)
    (if (equal? phi '(not #f))
    #t
    phi))

    (define (rewrite-rule-3 phi)
    (if (list? phi)
    (if (equal? (length phi) 2)
    (if (list? (second phi))
    (if (equal? (length (second phi)) 2)
    (if (and (equal? (first phi) 'not) (equal? (first (second phi)) 'not))
    (second (second phi))
    phi)
    phi)
    phi)
    phi)
    phi))

    (define (rewrite-rule-4 phi)
    (if (equal? phi '(and))
    #t
    (if (equal? phi '(or))
    #f
    phi)))

    (define (rewrite-rule-5 phi)
    (if (and (list? phi) (equal? (length phi) 2) (equal? (first phi) 'and))
    (if (list phi)
    (second phi)
    phi)
    (if (and (list? phi) (equal? (length phi) 2) (equal? (first phi) 'or))
    (if (list phi)
    (second phi)
    phi)
    phi)))

    (define (remove-true phi-list)
    (cond ((null? phi-list) phi-list)
    ((equal? (first phi-list) #t) (remove-true (rest phi-list)))
    (else (cons (first phi-list) (remove-true (rest phi-list))))))

    (define (remove-false phi-list)
    (cond ((null? phi-list) phi-list)
    ((equal? (first phi-list) #f) (remove-false (rest phi-list)))
    (else (cons (first phi-list) (remove-false (rest phi-list))))))

    (define (check-if-contains-true phi-list)
    (cond ((null? phi-list) #f)
    ((equal? (first phi-list) #t) #t)
    (else (check-if-contains-true (rest phi-list)))))

    (define (check-if-contains-false phi-list)
    (cond ((null? phi-list) #f)
    ((equal? (first phi-list) #f) #t)
    (else (check-if-contains-false (rest phi-list)))))

    (define (rewrite-rule-6 phi)
    (if (list? phi)
    (if (equal? (first phi) 'and)
    (if (check-if-contains-true phi)
    (remove-true phi)
    phi)
    (if (equal? (first phi) 'or)
    (if (check-if-contains-false phi)
    (remove-false phi)
    phi)
    phi))
    phi))

    (define (remove-and-formulas phi-list)
    (cond ((null? phi-list) '())
    ((and (list? (first phi-list)) (equal? (first (first phi-list)) 'and)) (append (rest (first phi-list)) (remove-and-formulas (rest phi-list))))
    (else phi-list)))

    (define (remove-or-formulas phi-list)
    (cond ((null? phi-list) '())
    ((and (list? (first phi-list)) (equal? (first (first phi-list)) 'or)) (append (rest (first phi-list)) (remove-and-formulas (rest phi-list))))
    (else phi-list)))

    (define (rewrite-rule-7 phi)
    (if (list? phi)
    (if (equal? (first phi) 'and)
    (if (check-if-contains-false phi)
    #f
    phi)
    (if (equal? (first phi) 'or)
    (if (check-if-contains-true phi)
    #t
    phi)
    phi))
    phi) )


    (define (remove-ands phi)
    (if (list? phi)
    (if (null? phi)
    phi
    (if (list? (first phi))
    (if (equal? (first (first phi)) 'and )
    (append (rest (first phi)) (remove-ands (rest phi)))
    (cons (first phi) (remove-ands (rest phi))))
    (cons (first phi) (remove-ands (rest phi)))))
    phi))

    (define (remove-ors phi)
    (if (list? phi)
    (if (null? phi)
    phi
    (if (list? (first phi))
    (if (equal? (first (first phi)) 'or )
    (append (rest (first phi)) (remove-ors (rest phi)))
    (cons (first phi) (remove-ors (rest phi))))
    (cons (first phi) (remove-ors (rest phi)))))
    phi))


    (define (rewrite-rule-8 phi)
    (if (list? phi)
    (if (equal? (first phi) 'and)
    (cons 'and (remove-ands (rest phi)))
    (if (equal? (first phi) 'or)
    (cons 'or (remove-ors (rest phi)))
    phi))
    phi))


    (define (remove-duplicates-from-given-list l)
    (cond ((null? l) l)
    ((member-exists (rest l) (first l)) (remove-duplicates-from-list (rest l)))
    (else (cons (first l) (remove-duplicates-from-list (rest l))))))

    (define (remove-duplicates-from-list l)
    (reverse (remove-duplicates-from-given-list (reverse l))))

    (define (rewrite-rule-9 phi)
    (if (list? phi)
    (if (or (equal? (first phi) 'and) (equal? (first phi) 'or))
    (remove-duplicates-from-list phi)
    phi)
    phi))


    (define (member-exists l x)
    (cond ((null? l) #f)
    ((equal? x (first l)) #t)
    (else (member-exists (rest l) x))))

    (define (check-fwd-opposite-existence l)
    (cond ((null? l) #f)
    ((member-exists (rest l) (cons 'not (list (first l)))) #t)
    (else (check-fwd-opposite-existence (rest l)))))

    (define (check-bwd-opposite-existence l)
    (cond ((null? l) #f)
    ((and (list? (first l)) (equal? (length (first l)) 2) (equal? (first (first l)) 'not)  (member-exists (rest l) (second (first l)))) #t)
    (else (check-bwd-opposite-existence (rest l)))))

    (define (check-opposite-existence l)
    (if (or (check-fwd-opposite-existence l) (check-bwd-opposite-existence l))
    #t
    #f))

    (define (rewrite-rule-10 phi)
    (if (list? phi)
    (if (and (equal? (first phi) 'and) (check-opposite-existence phi))
    #f
    (if (and (equal? (first phi) 'or) (check-opposite-existence phi))
    #t
    phi))
    phi))


    (define (total-rewrite phi)
    (rewrite-rule-9 (rewrite-rule-8 (rewrite-rule-10 (rewrite-rule-7 (rewrite-rule-6 (rewrite-rule-5 (rewrite-rule-4 (rewrite-rule-3 (rewrite-rule-2 (rewrite-rule-1 phi)))))))))) )

    (define (recursive-rewrite phi)
    (if (list? phi)
    (if (equal? phi (total-rewrite phi))
    phi
    (recursive-rewrite (total-rewrite phi)))
    phi))

    (define (list-simplification phi-list)
    (if (list? phi-list)
    (if (null? phi-list)
    phi-list
    (recursive-rewrite (map list-simplification phi-list)))
    (recursive-rewrite phi-list)))


    (define (boolean-simplify phi-list)
    (list-simplification phi-list))

;;Program FAILED test suite

;;The formula may be just a boolean value #t or #f.
;;You didn't consider this case.
;;Also, the two formulas may contain different symbols.
;;(truth-tables-match? '(or x y z (not z)) '(or a (not a)))
;;you will fail in this case


;;Grade: 80/100
