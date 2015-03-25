 ;; P is propositions= variables

    ;; A formula phi is
    ;; - a truth value
    ;; - a proposition
    ;; ()  // NOT phi
    ;; (phi1 ^ phi2)
    ;; (phi1 OR phi2)
    ;; ^ has precedence

    ;; Binging p -> t
    ;; p is a proposition
    ;; t is a truth value

    ;; Truth assignment I is a set of bindings
    ;; consistent if it does not map a proposition p to both true and false
    ;; complete if it maps every proposition in phi to a truth value

    ;; Valuation function assigns truth value to a formula pho given phi and I

    ;; define if a input is expression-constant-- number not required
    (define (expression-constant? con)
    (or (boolean? con)
    (number? con)
    (symbol? con)))

    ;; if the input is expression variable
    (define (expression-variable? var)
    (member var '(p1 p2 p3 p4 p5)))

    ;; if two bindings are inconsistent
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

    ;; combine result from two bindings
    (define (combine result1 result2)
    (if (or (eq? result1 #f) (eq? result2 #f) (inconsistent? result1 result2))
    '()
    (append result1 result2)))

    ;; instantiate
    (define (instantiate p bindings)
    (define (instantiate-with-bindings p) (instantiate p bindings))
    (cond ((expression-variable? p) (lookup p bindings))
    ((expression-constant? p) p)
    ((list? p) (map instantiate-with-bindings p))
    (else (panic "Invalid pattern"))))

    (define (find-first-matching-rule e rules)
    (if (null? rules)
    #f
    (if (eq? (match (first (first rules)) e) #f)
    (find-first-matching-rule e (rest rules))
    (first rules))))

    (define (lookup variable bindings)
    (cond ((null? bindings) (panic "Unbound variable"))
    ((eq? (first (first bindings)) variable) (second (first bindings)))
    (else (lookup variable (rest bindings)))))

    (define (match p e)
    (cond ((expression-variable? p) (list (list p e)))
    ((expression-constant? p) (if (equal? p e) '() #f))
    ((list? p)
    (if (and (list? e) (= (length p) (length e)))
    (map-reduce combine '() match p e)
    #f))
    (else (panic "Invalid pattern"))))

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
    (cond ((expression-variable? e)     (lookup e bindings))
    ((expression-constant? e) e)
    ((list? e)
    (case (first e)
    ((and) (map-reduce and-procedure #t evaluate-given-binds (rest e)))
    ((or)  (map-reduce or-procedure #f evaluate-given-binds (rest e)))
    ((not) (not (evaluate (second e) bindings)))
    (else "unknown operator")))
    (else "panic invalid expression")))

    (define (evaluateable-without-bindings e)
    (cond ((expression-variable? e)     #f)
    ((expression-constant? e) #t)
    ((list? e)
    (case (first e)
    ((and) (map-reduce and-procedure #t evaluateable-without-bindings (rest e)))
    ((or)  (map-reduce and-procedure #t evaluateable-without-bindings (rest e)))
    ((not) (map-reduce and-procedure #t evaluateable-without-bindings (rest e)))
    (else "unknown operator")))
    (else "panic invalid expression")))


    ;;(define (incorrect-evaluate e bindings)
    ;;(define (evaluate-given-binds e) (incorrect-evaluate e bindings))
    ;;(cond ((expression-variable? e)   (lookup e bindings))
    ;;((expression-constant? e) e)
    ;;((list? e)
    ;;(case (first e)
    ;;((and) (and (evaluate (second e) bindings) (evaluate (third e) bindings)))
    ;;((or)  (or (evaluate (second e) bindings) (evaluate (third e) bindings)))
    ;;((not) (not (evaluate (second e) bindings)))
    ;;(else "unknown operator")))
    ;;(else "panic invalid expression")))

    (define (is_bounded? bindings variable)
    (cond ((null? bindings) #f)
    ((eq? (first (first bindings)) variable) #t)
    (else (is_bounded? (rest bindings) variable))))

    (define (remove-bindings-redundancy bindings)
    (cond
    ((null? bindings) bindings)
    ((is_bounded? (rest bindings) (first (first bindings))) (remove-bindings-redundancy (rest bindings)))
    (else (cons (first bindings) (remove-bindings-redundancy (rest bindings))))))

    (define (remove-redundancy list_of_bindings)
    (cond
    ((null? list_of_bindings) list_of_bindings)
    (else (cons (remove-bindings-redundancy (first list_of_bindings)) (remove-redundancy (rest list_of_bindings))))))

    (define (update_binding_list binding_list new_bind)
    (cond ((null? binding_list) (list new_bind))
    ((null? (rest binding_list)) (list (append new_bind (first binding_list))))
    (else (cons (append new_bind (first binding_list))  (update_binding_list (rest binding_list) new_bind)))))

    (define (cross-product list1 list2)
    (cond
    ((null? list1) list2)
    ((null? list2) list1)
    ((null? (rest list1)) (update_binding_list list2  (first list1)))
    ((null? (rest list2)) (update_binding_list list1  (first list2)))
    (else (append (update_binding_list list1  (first list2)) (cross-product  list1 (rest list2))))))


    (define (create-binding-list current_list new_variable)
    (cond       ((null? current_list) (list (list (list new_variable #t)) (list (list new_variable #f))))
    ((is_bounded? (first current_list) new_variable) current_list)
    (else (cross-product (list (list (list new_variable #t)) (list (list new_variable #f))) current_list ))))

    (define (get-all-possible-bindings-with-duplicates e current_bindings)
    (cond ((expression-variable? e)
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

    (define (get-all-possible-bindings e)
    (remove-redundancy (remove-inconsistent (get-all-possible-bindings-with-duplicates e '()))))

    (define (remove-inconsistent list)
    (if (null? list)
    list
    (if (inconsistent? (first list) (first list))
    (remove-inconsistent (rest list))
    (cons (first list) (remove-inconsistent (rest list))))))


    (define (create-truth-table e possible-bindings)
    (if (null? possible-bindings)
    (if (evaluateable-without-bindings e) (list '((p1 #t)) (evaluate e '())) '())
    (cons (list (first possible-bindings) (evaluate e (first possible-bindings))) (create-truth-table e (rest possible-bindings)))))

    (define (truth-table phi)
    (create-truth-table phi (get-all-possible-bindings phi)))




Program FAILED test suite

you should extract the variables automatically from the input, instead of pre-defining

;;;;;;;;;;;;;;;;;;;; fixes to your code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    (define (expression-variable? var)
;;        (member var '(p1 p2 p3 p4 p5 a b c d e f)))
;;    test case: (truth-table '(and a b c d e f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Grade: 90/100	
