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
			  (const-table (const_table in-port))
			  (symbol-table (symbol_table in-port))
			  (global_env (global_env in-port))
			  (index -1))
			(display "const-table: ") (display const-table) (newline)
			(fprintf out-port "%include \"scheme.s\"") (newline out-port) (newline out-port)

			(cond ((not (null? const-table))
				(begin 
					(fprintf out-port "section .data") (newline out-port) (newline out-port)
					(fprintf out-port "print_format:") (newline out-port) 
					(fprintf out-port "\tdb \"%d\", 10, 0") (newline out-port) (newline out-port)
					(display (map (lambda (e) (set! index (+ index 1)) (create_const_for_assembly e index)) const-table))
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
					 
        


					;(fprintf (create_const_lines const-table) out-port)
					(fresh-line out-port)

			
			;(fresh-line out-port)
			;(fresh-line out-port)
			;(fprintf (symbol_table) out-port)
			(close-output-port out-port))))

;(define create_list_for_assembly
;	(lambda (lst)
;		))

(define break-list-to-components
	(lambda (lst len)
		(cond ((= len 0) (list '()))
			  (else (cons (list-head lst len) (break-list-to-components (cdr lst) (- len 1)))))))

(define expand-const-table
	(lambda (lst)
		(cond ((or (not (list? lst)) (null? lst)) lst)
			  ((and (list? lst) (> (length lst) 0) (not (list? (car lst)))) (cons (car lst) (expand-const-table (cdr lst))))
			  ((and (list? lst) (> (length lst) 0) (list? (car lst))) (append (break-list-to-components (car lst) (length (car lst))) (expand-const-table (cdr lst))))
		)))


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
	(lambda (con num)
		(cond ((integer? con) (list (string-append "const" (number->string num) ":" "\n" "\tdq MAKE_LITERAL(T_INTEGER, " (number->string con) ")")))
			  ((number? con) (list (string-append "const" (number->string num) ":" "\n" "\tdq MAKE_LITERAL(T_FRACTION, " (number->string con) ")")))
			  ((boolean? con) (if (equal? con #t)
			  					  (list (string-append "const" (number->string num) ":" "\n" "\tdq SOB_TRUE"))
			  					  (list (string-append "const" (number->string num) ":" "\n" "\tdq SOB_FALSE"))))
			  ((char? con) (list (string-append "const" (number->string num) ":" "\n" "\tdq MAKE_LITERAL(T_CHAR, " (string con) ")")))
			  ((string? con) (list (string-append "const" (number->string num) ":" "\n" "\tdq MAKE_LITERAL_STRING " con)))
			  ((null? con) (list (string-append "const" (number->string num) ":" "\n" "\tdq SOB_NIL")))
			  ((list? con) (create_list_for_assembly con)))))




