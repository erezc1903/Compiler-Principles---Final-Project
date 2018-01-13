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

(define expression_from_file
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
		(remove-duplicates (make_const_table (expression_from_file "input_file.scm")))))

(define make_symbol_table
	(lambda (exp)
		(cond ((and (list? exp) (null? exp)) exp)
			  ((and (list? exp) (or (equal? (car exp) 'fvar) (equal? (car exp) 'bvar) (equal? (car exp) 'pvar))) (list (cadr exp)))
			  ((and (list? exp) (list? (car exp))) (append (make_symbol_table (car exp)) (make_symbol_table (cdr exp))))
			  ((and (list? exp) (> (length exp) 0) (not (list? (car exp)))) (make_symbol_table (cdr exp))))))

(define symbol_table
	(lambda ()
		(remove-duplicates (make_symbol_table (expression_from_file "input_file.scm")))))


