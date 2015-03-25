    ;; board is a list of lists
    ;; each item of board is row

    (define (make-list item length)
    (if (zero? length)
    '()
    (cons item (make-list item (- length 1)))))

    (define (initial-board n)
    (make-list (make-list 0 n) n))

    (define (get-ith-element input_list i)
    (if (zero? i)
    (first input_list)
    (get-ith-element (rest input_list) (- i 1))))

    (define (get-board-element board row column)
    (get-ith-element (get-ith-element board row) column))

    (define (count input_list element)
    (cond ((null? input_list) 0)
    ((equal? (first input_list) element) (+ 1 (count (rest input_list) element)))
    (else (count (rest input_list) element)))

    )

    (define (count-X board)
    (if (null? board)
    0
    (+ (count (first board) 1) (count-X (rest board)))))

    (define (count-O board)
    (if (null? board)
    0
    (+ (count (first board) -1) (count-O (rest board)))))

    (define (count-zero board)
    (if (null? board)
    0
    (+ (count (first board) 0) (count-zero (rest board)))))


    (define (whose-move? board)
    (cond ((null? board) 0)
    ((equal? (count-X board) (count-O board)) 1)
    ((equal? (count-X board) (+ (count-O board) 1)) -1)
    (else 0)))

    (define (add-1 value)
    (+ 1 value))

    (define (add-row-coordinate x-coordinate y-coordinates)
    (if (null? y-coordinates)
    '()
    (cons (list x-coordinate (first y-coordinates)) (add-row-coordinate x-coordinate (rest y-coordinates)))))

    (define (get-element-positions list element)
    (cond ((null? list) '())
    ((equal? element (first list)) (cons 0 (map add-1 (get-element-positions (rest list) element))))
    (else  (map add-1 (get-element-positions (rest list) element)))) )

    (define (get-element-coordinates-row board element row_number)
    (cond ((null? board) '())
    (else (append (add-row-coordinate row_number (get-element-positions (first board) element)) (get-element-coordinates-row (rest board) element (+ 1 row_number))))))

    (define (get-element-coordinates board element)
    (get-element-coordinates-row board element 0))

    (define (moves board)
    (get-element-coordinates board 0))

    (define (update-column-element given_row column new_element)
    (cond ((null? given_row) given_row)
    ((zero? column) (cons new_element (rest given_row)))
    (else (cons (first given_row) (update-column-element (rest given_row) (- column 1) new_element)))))


    (define (update-board-element board row column new_element)
    (cond ((null? board) board)
    ((zero? row) (cons (update-column-element (first board) column new_element) (rest board)))
    (else (cons (first board) (update-board-element (rest board) (- row 1) column new_element)))))

    (define (make-move m board)
    (cond ((null? m) board)
    ((equal? (whose-move? board) 1) (update-board-element board (first m) (second m) 1))
    ((equal? (whose-move? board) -1) (update-board-element board (first m) (second m) -1))
    (else (panic "whose-move? returned 0"))))

    (define (make-move! m board player)
    (update-board-element board (first m) (second m) player))


    (define (make-moves m_list board player)
    (if (null? m_list)
    board
    (make-moves (rest m_list) (make-move! (first m_list) board player) player)))

    (define (check-all-elements-equal-to l x)
    (cond ((null? l) #t)
    ((equal? (first l) x) (check-all-elements-equal-to (rest l) x))
    (else #f)))

    (define (check-row-win board)
    (cond ((null? board) 0)
    ((check-all-elements-equal-to (first board) -1) -1)
    ((check-all-elements-equal-to (first board) 1) 1)
    (else (check-row-win (rest board)))))

    (define (count-row-win board)
    (cond ((null? board) 0)
    ((check-all-elements-equal-to (first board) -1) (+ -1 (count-row-win (rest board))))
    ((check-all-elements-equal-to (first board) 1) (+ 1 (count-row-win (rest board))))
    (else (count-row-win (rest board)))))

    (define (convert-column-to-list board column)
    (if (null? board)
    '()
    (cons (get-board-element board 0 column) (convert-column-to-list (rest board) column))))

    (define (get-number-of-columns board)
    (length (first board)))

    (define (get-number-of-rows board)
    (length board))

    (define (check-column-win2 board start_column)
    (cond ((>= start_column (get-number-of-columns board))      0)
    ((check-all-elements-equal-to (convert-column-to-list board start_column) -1) -1)
    ((check-all-elements-equal-to (convert-column-to-list board start_column) 1) 1)
    (else (check-column-win2 board (+ 1 start_column)))))

    (define (check-column-win board)
    (check-column-win2 board 0))

    (define (count-column-win2 board start_column)
    (cond ((>= start_column (get-number-of-columns board))      0)
    ((check-all-elements-equal-to (convert-column-to-list board start_column) 1) (+ 1 (count-column-win2 board (+ 1 start_column))))
    ((check-all-elements-equal-to (convert-column-to-list board start_column) -1) (+ -1 (count-column-win2 board (+ 1 start_column))))
    (else  (count-column-win2 board (+ 1 start_column)))))

    (define (count-column-win board)
    (count-column-win2 board 0))

    (define (convert-dia1-to-list board row)
    (if (>= row (get-number-of-rows board))
    '()
    (cons (get-board-element board row row) (convert-dia1-to-list board (+ 1 row)))))

    (define (convert-dia2-to-list board row)
    (if (>= row (get-number-of-rows board))
    '()
    (cons (get-board-element board row (- (get-number-of-columns board) (+ 1 row))) (convert-dia2-to-list board (+ 1 row)))))

    (define (check-dia-1-win board)
    (cond ((check-all-elements-equal-to (convert-dia1-to-list board 0) -1) -1)
    ((check-all-elements-equal-to (convert-dia1-to-list board 0) 1) 1)
    (else 0)))

    (define (check-dia-2-win board)
    (cond ((check-all-elements-equal-to (convert-dia2-to-list board 0) -1) -1)
    ((check-all-elements-equal-to (convert-dia2-to-list board 0) 1) 1)
    (else 0)))

    (define (win board)
    (cond ((equal? (check-row-win board) 1) 1)
    ((equal? (check-row-win board) -1) -1)
    ((equal? (check-column-win board) 1) 1)
    ((equal? (check-column-win board) -1) -1)
    ((equal? (check-dia-1-win board) -1) -1)
    ((equal? (check-dia-1-win board) 1) 1)
    ((equal? (check-dia-2-win board) -1) -1)
    ((equal? (check-dia-2-win board) 1) 1)
    (else 0)))

    (define (win-count board)
    (+ (check-dia-1-win board) (check-dia-2-win board) (count-row-win board) (count-column-win board)))

    (define (get-board-fill-X board)
    (make-moves (moves board) board 1))

    (define (get-board-fill-O board)
    (make-moves (moves board) board -1))

    (define (static-evaluator board)
    (cond ((equal? (win board) 1) 1)
    ((equal? (win board) -1) -1)
    ((equal? 0 (- (win-count (get-board-fill-X board)) (win-count (get-board-fill-O board)))) 0)
    ((equal? (win-count (get-board-fill-X board)) 0) -0.9)
    ((equal? (win-count (get-board-fill-O board)) 0) 0.9)
    (else  (/ (+ (win-count (get-board-fill-X board)) (win-count (get-board-fill-O board))) (- (win-count (get-board-fill-X board)) (win-count (get-board-fill-O board)))))))

    (define (try-a-list-of-moves board move-list)
    (if (null? move-list)
    '()
    (cons (make-move (first move-list) board) (try-a-list-of-moves board (rest move-list)))))

    (define (get-new-board-positions board )
    (try-a-list-of-moves board (moves board)))

    (define (multiply-list-by-const list const)
    (if (null? list)
    '()
    (cons (* const (first list)) (multiply-list-by-const (rest list) const))))

    (define (max-of-list l)
    (cond ((null? l) '())
    ((equal? (length l) 1) (first l))
    ((> (first l) (max-of-list (rest l))) (first l))
    (else (max-of-list (rest l)))))

    (define (board-evaluator~ k board)
    (define (board-evaluator-with-k board) (board-evaluator~ (- k 1) board))
    (cond ((equal? (win board) 1) 1)
    ((equal? (win board) -1) -1)
    ((equal? k 0) (static-evaluator board))
    (else (* (whose-move? board) (max-of-list (multiply-list-by-const (map board-evaluator-with-k (try-a-list-of-moves board (moves board))) (whose-move? board)))))))

    (define (arg-max-pair moves values max_value)
    (cond ((null? values) '())
    ((equal? (first values) max_value) (cons (first moves) (arg-max-pair (rest moves) (rest values) max_value)))
    (else (arg-max-pair (rest moves) (rest values) max_value))))

    (define (map-pruning f l player)
    (cond ((null? l) l)
    ((equal? (f (first l)) player) (list player))
    (else (cons (f (first l)) (map-pruning f (rest l) player)))))

    (define (best-move-pruned-values k board)
    (define (board-evaluator-with-k board) (board-evaluator~ (- k 1) board))
    (cond ((equal? (win board) 1) '())
    ((equal? (win board) -1) '() )
    ((equal? k 0) (list '() (static-evaluator board)))
    (else
    (let ((m (moves board)))
    (let ((values (multiply-list-by-const (map-pruning board-evaluator-with-k (try-a-list-of-moves board m) (whose-move? board)) (whose-move? board))))
    (let ((max_value (max-of-list values)))
    (list values )))))))

    (define (best-move-values k board)
    (define (board-evaluator-with-k board) (board-evaluator~ (- k 1) board))
    (cond ((equal? (win board) 1) '())
    ((equal? (win board) -1) '() )
    ((equal? k 0) (list '() (static-evaluator board)))
    (else
    (let ((m (moves board)))
    (let ((values (multiply-list-by-const (map board-evaluator-with-k (try-a-list-of-moves board m)) (whose-move? board))))
    (let ((max_value (max-of-list values)))
    (list values)))))))


    ;; this is actually inefficient :-/
    (define (map-pruning-efficient f board player moves)
    (cond ((null? moves) (list moves))
    ((equal? (f (make-move (first moves) board)) player) (list (first moves)))
    ((null? (rest moves)) (list (first moves)))
    ((> (* player (f (make-move (first moves) board))) (* player (f (make-move (first (map-pruning-efficient f board player (rest moves))) board ))))  (list (first moves)))
    (else (map-pruning-efficient f board player (rest moves)))))


    (define (best-move-pruned-bad k board)
    (define (board-evaluator-with-k board) (board-evaluator~ (- k 1) board))
    (cond ((equal? (win board) 1) '())
    ((equal? (win board) -1) '() )
    ((equal? k 0) (list '() (static-evaluator board)))
    (else (map-pruning-efficient board-evaluator-with-k board (whose-move? board) (moves board)))))

    (define (best-move-pruned k board)
    (define (board-evaluator-with-k board) (board-evaluator~ (- k 1) board))
    (cond ((equal? (win board) 1) '())
    ((equal? (win board) -1) '() )
    ((equal? k 0) (if (null? (moves board)) '() (first (moves board))))
    (else
    (let ((m (moves board)))
    (let ((values (multiply-list-by-const (map-pruning board-evaluator-with-k (try-a-list-of-moves board m) (whose-move? board)) (whose-move? board))))
    (let ((max_value (max-of-list values)))
    (arg-max-pair m values max_value)))))))


    (define (best-move k board)
    (define (board-evaluator-with-k board) (board-evaluator~ (- k 1) board))
    (cond ((equal? (win board) 1) '())
    ((equal? (win board) -1) '() )
    ((equal? k 0) (list '() (static-evaluator board)))
    (else
    (let ((m (moves board)))
    (let ((values (multiply-list-by-const (map board-evaluator-with-k (try-a-list-of-moves board m)) (whose-move? board))))
    (let ((max_value (max-of-list values)))
    (arg-max-pair m values max_value)))))))

    (define (optimal-moves~ k b)
    (cond ((equal? k infinity) (best-move-pruned (count-zero b) b))
    ((> k (count-zero b)) (best-move-pruned (count-zero b) b))
    (else (best-move k b))))

    (define (optimal-moves k b)
    (optimal-moves~ k b))


;;Program PASSED test suite

;;Grade: 100/100
