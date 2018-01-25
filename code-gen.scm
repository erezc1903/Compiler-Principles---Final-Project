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

(define string->file
	(lambda (out-file)
		(let ((out-port (open-output-file out-file))
			  (const-table (const_table))
			  (symbol-table (symbol_table)))

			(cond ((not (null? const-table))
				(begin 
					(write "section .data:" out-port)
					(fresh-line out-port)
					(map (lambda (e) (set! index (+ index 1)) (write (create_const_for_assembly e index) out-port) (newline out-port) (newline out-port)) const-table)
					;(write (create_const_lines const-table) out-port)
					(fresh-line out-port))))

			
			;(fresh-line out-port)
			;(fresh-line out-port)
			;(write (symbol_table) out-port)
			(close-output-port out-port))))


(define expressions_from_file
	(lambda (file_name)
		(pipeline (file->list file_name))))

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
	(lambda ()
		(remove-duplicates (make_const_table (expressions_from_file "input_file.scm")))))

(define make_symbol_table
	(lambda (exp)
		(cond ((and (list? exp) (null? exp)) exp)
			  ((and (list? exp) (or (equal? (car exp) 'fvar) (equal? (car exp) 'bvar) (equal? (car exp) 'pvar))) (list (cadr exp)))
			  ((and (list? exp) (list? (car exp))) (append (make_symbol_table (car exp)) (make_symbol_table (cdr exp))))
			  ((and (list? exp) (> (length exp) 0) (not (list? (car exp)))) (make_symbol_table (cdr exp))))))

(define symbol_table
	(lambda ()
		(remove-duplicates (make_symbol_table (expressions_from_file "input_file.scm")))))

(define create_const_for_assembly
	(lambda (con num)
		(cond ((integer? con) (string-append "sobInt" (number->string num) ": dq MAKE_LITERAL(T_INTEGER, " (number->string con) ")"))
			  ((number? con) (string-append "const" (number->string num) ": dq MAKE_LITERAL(T_FRACTION, " (number->string con) ")"))
			  ((boolean? con) (string-append "const" (number->string num) ": dq MAKE_LITERAL(T_BOOL, " (format "~a" con) ")"))
			  ((char? con) (string-append "const" (number->string num) ": dq MAKE_LITERAL(T_CHAR, " (string con) ")"))
			  )))

(define index -1)

(define create_const_lines
	(lambda (const-table)
		(map (lambda (e) (set! index (+ index 1)) (create_const_for_assembly e index)) const-table)))


