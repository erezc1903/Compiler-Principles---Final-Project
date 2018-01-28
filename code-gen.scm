(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")

(define pipeline
	(lambda (s)
		((star <sexpr>) s
		 (lambda (m r)
		    (map (lambda (e)
					(annotate-tc
					  (pe->lex-pe
						(box-set
						  (remove-applic-lambda-nil
							(parse e))))))
			 m))
		(lambda (f) 'fail))))



(define file->list
	(lambda (in-file)
		(let ((in-port (open-input-file in-file)))
			(letrec ((run
					  (lambda ()
						(let ((ch (read-char in-port)))
							(if (eof-object? ch)
								(begin
								 (close-input-port in-port)
								 '())
								(cons ch (run)))))))
				(run)))))

(define compile-scheme-file
	(lambda (in-file out-file)
		(let* ((in-port (pipeline (file->list in-file)))
			  (out-port (open-output-file out-file 'replace))
			  (index -1)
			  (const-table (const_table in-port))
			  (const-table-as-list-of-pairs (map (lambda (e) (set! index (+ index 1)) (pairs_of_name_and_object e index)) const-table))
			  (symbol-table (symbol_table in-port))
			  (global_env (global_env in-port)))

			  (display "const-table: ") (display const-table) (newline)
			(fprintf out-port "%include \"scheme.s\"") (newline out-port) (newline out-port)
			(display "pairs_of_name_and_object: ") (display const-table-as-list-of-pairs) (newline) (newline)
			(cond ((not (null? const-table))
				(begin 
					(fprintf out-port "section .data") (newline out-port) (newline out-port)
					(fprintf out-port "print_format:") (newline out-port) 
					(fprintf out-port "\tdb \"%d\", 10, 0") (newline out-port) (newline out-port)
					(set! index -1)
					(map (lambda (e) (set! index (+ index 1)) (fprintf out-port (create_const_for_assembly e const-table-as-list-of-pairs index))) const-table)
					(set! index -1))))
			(fprintf out-port "section .bss") (newline out-port) (newline out-port)
			(fprintf out-port "section .text") (newline out-port) (newline out-port)
			(fprintf out-port "\textern exit, printf, scanf, malloc") (newline out-port) (newline out-port)
			(fprintf out-port "\tglobal main") (newline out-port) (newline out-port)
			(fprintf out-port "main:") (newline out-port) (newline out-port)
			(fprintf out-port "\tpush rbp") (newline out-port)
			(fprintf out-port "\tmov rax, qword [const2]") (newline out-port)
			(fprintf out-port "\tpush rax") (newline out-port)
			(fprintf out-port "\tcall write_sob") (newline out-port)
			(fprintf out-port "\tadd rsp, 8") (newline out-port)
			(fprintf out-port "\tpop rbp") (newline out-port)
			(fprintf out-port "\tret") (newline out-port)
			(fresh-line out-port)

			
			;(fresh-line out-port)
			;(fresh-line out-port)
			;(fprintf (symbol_table) out-port)
			(close-output-port out-port))))

;(define create_list_for_assembly
;	(lambda (lst)
;		(cond ((null? lst) "sobNil")
;			  ((= (length lst) 1) (list (string-append "sobPair" (convert-lst-to-string lst) "N")))
;			  ((> (length lst) 1) (list lst (string-append "sobPair" (convert-lst-to-string lst) " (" (create_car_type_for_list (car lst)) ", sobPair" (convert-lst-to-string (cdr lst)) ")") (create_list_for_assembly (cdr lst))))))


(define break-list-to-components
	(lambda (lst)
		(display "break-list-to-components") (newline) 
		(display "break-list-to-components lst: ") (display lst) (newline) (newline)
		(cond ((vector? lst) (expand-vector (vector->list lst)))
			  ((or (not (pair? lst)) (null? lst)) lst)
			  ((or (list? (car lst)) (pair? (car lst))) (append (expand-const-table (list (car lst))) (list (cdr lst)) (break-list-to-components (cdr lst))))
			  (else (append (list (car lst)) (list (cdr lst)) (break-list-to-components (cdr lst)))))))

(define expand-const-table
	(lambda (lst)
		(display "expand-const-table") (newline) 
		(display "expand-const-table lst: ") (display lst) (newline) (newline)
		(cond ((and (list? lst) (> (length lst) 0) (vector? (car lst))) 
					(append (vector->list (car lst)) (expand-vector (vector->list (car lst))) (list (car lst)) (expand-const-table (cdr lst))))		
			  ((and (list? lst) (> (length lst) 0) (or (list? (car lst)) (pair? (car lst)))) (append (break-list-to-components (car lst)) (list (car lst)) (expand-const-table (cdr lst))))
			  ((and (list? lst) (> (length lst) 0) (not (list? (car lst)))) (cons (car lst) (expand-const-table (cdr lst))))
			  ;((vector? lst) (map expand-const-table (vector->list lst)))
			  (else lst)
		)))

(define expand-vector
	(lambda (vec) 
		(display "expand-vector: ") (display vec) (newline) (newline)
					(cond ((null? vec) vec)
						((or (list? (car vec)) (pair? (car vec))) (append (break-list-to-components (car vec)) (expand-const-table (cdr vec))))
						((vector? (car vec)) (append (expand-vector (vector->list (car vec))) (expand-const-table (cdr vec))))
						(else (cons (expand-const-table (car vec)) (expand-vector (cdr vec)))))))

(define make_const_table
	(lambda (exp)
		(cond ((and (list? exp) (null? exp)) exp)
			  ((and (list? exp) (= (length exp) 2) (equal? (car exp) 'const)) (list (cadr exp)))
			  ((and (list? exp) (list? (car exp))) (append (make_const_table (car exp)) (make_const_table (cdr exp))))
			  ((and (list? exp) (> (length exp) 0) (not (list? (car exp)))) (make_const_table (cdr exp))))))

(define remove-duplicates
	(lambda (exp)
		(cond ((null? exp) exp)
			  ((member (car exp) (cdr exp)) (remove-duplicates (cdr exp)))
			  (else (cons (car exp) (remove-duplicates (cdr exp)))))))

(define const_table
	(lambda (input-file)
		(display "const-table: ") (display (make_const_table input-file)) (newline) (newline)
		(display "input-file: ") (display input-file) (newline) (newline)

		(remove-duplicates (expand-const-table (make_const_table input-file)))))

(define make_symbol_table
	(lambda (exp)
		(cond ((and (list? exp) (null? exp)) exp)
			  ((and (list? exp) (or (equal? (car exp) 'bvar) (equal? (car exp) 'pvar))) (list (cadr exp)))
			  ((and (list? exp) (list? (car exp))) (append (make_symbol_table (car exp)) (make_symbol_table (cdr exp))))
			  ((and (list? exp) (> (length exp) 0) (not (list? (car exp)))) (make_symbol_table (cdr exp))))))

(define symbol_table
	(lambda (input-file)
		(remove-duplicates (make_symbol_table input-file))))

(define make_global_env
	(lambda (exp)
		(cond ((and (list? exp) (null? exp)) exp)
			  ((and (list? exp) (equal? (car exp) 'fvar)) (list (cadr exp)))
			  ((and (list? exp) (list? (car exp))) (append (make_global_env (car exp)) (make_global_env (cdr exp))))
			  ((and (list? exp) (> (length exp) 0) (not (list? (car exp)))) (make_global_env (cdr exp))))))

(define global_env
	(lambda (input-file)
		(remove-duplicates (make_global_env input-file))))

(define create_const_for_assembly
	(lambda (con table-in-pairs num)
		;(display "create_const_for_assembly con : ") (display con) (newline)
		;(display "create_const_for_assembly list: ") (display table-in-pairs) (newline) (newline)
		(cond ((integer? con) (string-append (find-const-in-pairs con table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL(T_INTEGER, " (number->string con) ")\n\n"))
			  ((number? con) (string-append (find-const-in-pairs con table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL(T_FRACTION, " (number->string con) ")\n\n"))
			  ((boolean? con) (if (equal? con #t)
			  					  (string-append "sobTrue:" "\n" "\tdq SOB_TRUE\n\n")
			  					  (string-append "sobFalse:" "\n" "\tdq SOB_FALSE\n")))
			  ((char? con) (string-append (find-const-in-pairs con table-in-pairs) "\n" "\tdq MAKE_LITERAL(T_CHAR, " (string con) ")\n\n"))
			  ((symbol? con) (string-append (find-const-in-pairs con table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL(T_SYMBOL, " (symbol->string con) ")\n\n"))
			  ((string? con) (string-append (find-const-in-pairs con table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL_STRING \"" con "\"\n\n"))
			  ((null? con) (string-append "sobNil:" "\n" "\tdq SOB_NIL\n\n"))
			  ((vector? con) (string-append (find-const-in-pairs con table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL_VECTOR " (trim-last-comma (get-components-from-pairs con table-in-pairs)) "\n\n"))
			  ((and (list? con) (> (length con) 1) (list? (cadr con))) (string-append (find-const-in-pairs con table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL_PAIR (" (find-const-in-pairs (car con) table-in-pairs) ", " (find-const-in-pairs (cadr con) table-in-pairs) ")\n\n"))
			  ((or (list? con) (pair? con)) (string-append (find-const-in-pairs con table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL_PAIR(" (find-const-in-pairs (car con) table-in-pairs) ", " (find-const-in-pairs (cdr con) table-in-pairs) ")\n\n"))
			  )))

(define trim-last-comma
	(lambda (str)
		(display "trim-last-comma: ") (display str) (newline) (newline)
		(substring str 0 (- (string-length str) 2))))

(define find-const-in-pairs
	(lambda (const table-in-pairs)
		;(display "find-const-in-pairs const : ") (display const) (newline)
		;(display "find-const-in-pairs list: ") (display table-in-pairs) (newline) 
		;(display "find-const-in-pairs cadar list : ") (display (cadar table-in-pairs)) (newline) (newline)
		(cond ((equal? const (cadar table-in-pairs)) (caar table-in-pairs))
			  ((> (length table-in-pairs) 0) (find-const-in-pairs const (cdr table-in-pairs))))))

(define get-components-from-pairs
	(lambda (vec table-in-pairs)
		(cond ((null? table-in-pairs) "")
			  ((member (cadar table-in-pairs) (vector->list vec)) (string-append (caar table-in-pairs) ", " (get-components-from-pairs vec (cdr table-in-pairs))))
			  (else (get-components-from-pairs vec (cdr table-in-pairs))))))


(define pairs_of_name_and_object
	(lambda (con num)
		(cond ((integer? con) (list (string-append "sobInt" (number->string con)) con))
			  ((number? con) (list (string-append "sobFrac" (number->string con)) con))
			  ((boolean? con) (if (equal? con #t)
			  					  (list "sobTrue" con)
			  					  (list "sobFalse" con)))
			  ((char? con) (list (string-append "sobChar" (number->string num)) con))
			  ((string? con) (list (string-append "sobString" (number->string num)) con))
			  ((null? con) (list "sobNil" '()))
			  ((vector? con) (list (string-append "sobVector" (number->string num)) con))
			  ((symbol? con) (list (string-append "sobSymbol" (symbol->string con)) con))
			  ((or (list? con) (pair? con)) (list (string-append "sobPair" (number->string num)) con))
			  )))



;;(#t av at me r 1 (2 3) 2 (3) 3 () (1 2 3) 4 (5 6 . #(4 8 16 (1 2 3))) 5 (6 . #(4 8 16 (1 2 3)))
; 6 #(4 8 16 (1 2 3)) (4 8 16 (1 2 3) #(4 8 16 (1 2 3))) (4 5 6 . #(4 8 16 (1 2 3))))