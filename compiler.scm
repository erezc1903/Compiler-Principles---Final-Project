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


(define code-gen
	  (lambda (pe const-table global-env)
	  	;(display "const-table in code-gen: ") (display const-table) (newline)
	  	;(display "pe in code-gen: ") (display pe) (newline) (newline)
	      (string-append  
	      	(cond ((tagged-by? pe 'const) (string-append "\tmov rax, qword [" (find-const-in-pairs (cadr pe) const-table) "]\n"))
			       ((tagged-by? pe 'if3) (handle_if pe const-table global-env))
			       ((tagged-by? pe 'seq) (handle_seq pe const-table global-env))
			       ((tagged-by? pe 'or) (handle_or (cadr pe) (make-end-label-for-or) const-table global-env))
			       ((tagged-by? pe 'def) (handle_define (cdr pe) const-table global-env))
			       ;((tagged-by? pe 'applic) (handle_applic pe))
			       ;((tagged-by? pe 'tc-applic) (handle_tc_applic pe))
			       ;((tagged-by? pe 'lambda-simple) (handle_lambda_simple pe))
			       ;((tagged-by? pe 'lambda-opt) (handle_lambda_opt pe))
			       ;((tagged-by? pe 'lambda-var) (handle_lambda_var pe))
			       ;((tagged-by? pe 'pvar) (handle_pvar_get pe))
			       ;((tagged-by? pe 'bvar) (handle_bvar_get pe))
			       ;((tagged-by? pe 'fvar) (handle_fvar_get pe))
			       ;((tagged-by? pe 'set) (cond ((tagged-by? (cadr pe) 'pvar) (handle_pvar_set pe))
							   ;((tagged-by? (cadr pe) 'bvar) (handle_bvar_set pe))
							   ;((tagged-by? (cadr pe) 'fvar) (handle_fvar_set pe))))
			       ;((tagged-by? pe 'box-set) (cond ((tagged-by? (cadr pe) 'pvar) (handle_pvar_boxset pe))
							   ;    ((tagged-by? (cadr pe) 'bvar) (handle_bvar_boxset pe))))
			       ;((tagged-by? pe 'box-get) (cond ((tagged-by? (cadr pe) 'pvar) (handle_pvar_boxget pe))
							   ;    ((tagged-by? (cadr pe) 'bvar) (handle_bvar_boxget pe))))
			       ;((tagged-by? pe 'box) (handle_box pe))
			       (else "")))
	      ;(set! num (+ num 1))
	  ))

(define compile-scheme-file
	(lambda (in-file out-file)
		(let* ((input (pipeline (file->list in-file)))
			  (out-port (open-output-file out-file 'replace))
			  (index -1)
			  (const-table (const_table input))
			  (const-table-as-list-of-pairs (map (lambda (e) (set! index (+ index 1)) (pairs_of_name_and_object e index)) const-table))
			  (global-env (global_env input)))
			  ;(symbol-table (symbol_table input))

			;(display "input: ") (display input) (newline) (newline)
			(fprintf out-port "%include \"scheme.s\"\n\n") 
			
			(fprintf out-port "section .bss\n\n") 
			
			(fprintf out-port "section .data\n\n") 

			(fprintf out-port (create_global_env_for_assembly global-env))

			(fprintf out-port (create_const_for_assembly const-table const-table-as-list-of-pairs 0))

			(fprintf out-port "\n\nsection .text\n\n")
			(fprintf out-port "\textern exit, printf, scanf, malloc\n\n")
			(fprintf out-port "\tglobal main\n\n")
			(fprintf out-port "main:\n\n")
			
			(map (lambda (pe) (fprintf out-port (code-gen pe const-table-as-list-of-pairs global-env))) input) 

			(fprintf out-port "\tpush rbp") (newline out-port)
			(fprintf out-port "\tpush rax") (newline out-port)
			(fprintf out-port "\tcall write_sob_if_not_void") (newline out-port)
			(fprintf out-port "\tadd rsp, 8") (newline out-port)
			(fprintf out-port "\tpop rbp") (newline out-port)
			(fprintf out-port "\tret\n\n") 

			(close-output-port out-port))))


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR DEF EXPRESSION ====================================================
;=========================================================================================================================================

(define handle_define
	(lambda (def-exp const-table global-env)
		(let ((free-var (find-var-in-global-env (car def-exp) global-env)))
				(string-append 
					(code-gen (cadr def-exp) const-table global-env)
					"\tmov rbx, " (symbol->string free-var) "\n" 
					"\tmov qword [rbx], rax\n\n"))))

(define find-var-in-global-env
	(lambda (var global-env)
		(if (equal? (cadr var) (car global-env))
			(cadr var)
			(find-var-in-global-env var (cdr global-env)))))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR DEF EXPRESSION =============================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR SEQ EXPRESSION ====================================================
;=========================================================================================================================================

(define handle_seq
  (lambda (seq-exp const-table global-env)
    (fold-left (lambda (res exp) (string-append res (code-gen exp const-table global-env))) "" (cadr seq-exp))
    ))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR SEQ EXPRESSION =============================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR OR EXPRESSION =====================================================
;=========================================================================================================================================


(define handle_or
		(lambda (or-exp end-label const-table global-env)
				(if (null? or-exp) 
					(string-append end-label ":\n\n")
					(string-append 
						(code-gen (car or-exp) const-table global-env)
						"\tcmp rax, SOB_FALSE\n" 
						"\tjne " end-label "\n"
						(handle_or (cdr or-exp) end-label const-table global-env)))))

(define make-end-label-for-or
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "Lend" (number->string num)))))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR OR EXPRESSION ==============================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR IF EXPRESSION =====================================================
;=========================================================================================================================================

(define handle_if
		(lambda (if-exp const-table global-env)
			;(display "if-exp in handle_if: ") (display if-exp) (newline) 
			;(display "cadr if-exp in handle_if: ") (display (code-gen (cadr if-exp) const-table)) (newline)
			;(display "caddr if-exp in handle_if: ") (display (code-gen (caddr if-exp) const-table)) (newline)
			;(display "cadddr if-exp in handle_if: ") (display (code-gen (cadddr if-exp) const-table)) (newline) (newline)
			(let ((dif-label (make-dif-label))
				  (end-label (make-end-label)))
				 (string-append 
					(code-gen (cadr if-exp) const-table global-env) 
					"\tcmp rax, SOB_FALSE\n" 
					"\tje " dif-label "\n"
					(code-gen (caddr if-exp) const-table global-env) "\n"
					"\tjmp " end-label "\n"
					dif-label ":\n"
					"\t" (code-gen (cadddr if-exp) const-table global-env) "\n"
					end-label ":\n\n")
					)))


(define make-dif-label
	(let ((num -1))
		(lambda ()
			(set! num (+ num 1))
			(string-append "L" (number->string num)))))

(define make-end-label
	(let ((num -1))
			(lambda ()
				(set! num (+ num 1))
				(string-append "Lend" (number->string num)))))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR IF EXPRESSION ==============================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR CONSTANT TABLE ====================================================
;=========================================================================================================================================
(define break-list-to-components
	(lambda (lst)
		;(display "break-list-to-components") (newline) 
		;(display "break-list-to-components lst: ") (display lst) (newline) (newline)
		(cond ((vector? lst) (expand-vector (vector->list lst)))
			  ((or (not (pair? lst)) (null? lst)) lst)
			  ((or (list? (car lst)) (pair? (car lst))) (append (expand-const-table (list (car lst))) (list (cdr lst)) (break-list-to-components (cdr lst))))
			  (else (append (list (car lst)) (list (cdr lst)) (break-list-to-components (cdr lst)))))))

(define expand-const-table
	(lambda (lst)
		;(display "expand-const-table") (newline) 
		;(display "expand-const-table lst: ") (display lst) (newline) (newline)
		(cond ((and (list? lst) (> (length lst) 0) (vector? (car lst))) 
					(append (vector->list (car lst)) (expand-vector (vector->list (car lst))) (list (car lst)) (expand-const-table (cdr lst))))		
			  ((and (list? lst) (> (length lst) 0) (or (list? (car lst)) (pair? (car lst)))) (append (break-list-to-components (car lst)) (list (car lst)) (expand-const-table (cdr lst))))
			  ((and (list? lst) (> (length lst) 0) (not (list? (car lst)))) (cons (car lst) (expand-const-table (cdr lst))))
			  ;((vector? lst) (map expand-const-table (vector->list lst)))
			  (else lst)
		)))

(define expand-vector
	(lambda (vec) 
		;(display "expand-vector: ") (display vec) (newline) (newline)
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


(define const_table
	(lambda (input-file)
		;(display "const-table: ") (display (make_const_table input-file)) (newline) (newline)
		;(display "input-file: ") (display input-file) (newline) (newline)
		(remove-duplicates (expand-const-table (make_const_table input-file)))))

(define create_const_for_assembly
	(lambda (const-table table-in-pairs num)
		;(display "create_const_for_assembly const-table : ") (display const-table) (newline)
		;(display "create_const_for_assembly table-in-pairs: ") (display table-in-pairs) (newline) (newline)
		(set! num (+ num 1))
		(cond ((null? const-table) "")
			  ((integer? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL(T_INTEGER, " (number->string (car const-table)) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((number? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL(T_FRACTION, " (number->string (car const-table)) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((boolean? (car const-table)) (if (equal? (car const-table) #t)
			  					  (string-append "sobTrue:" "\n" "\tdq SOB_TRUE\n\n" (create_const_for_assembly (cdr const-table) (cdr table-in-pairs) num))
			  					  (string-append "sobFalse:" "\n" "\tdq SOB_FALSE\n" (create_const_for_assembly (cdr const-table) (cdr table-in-pairs) num))))
			  ((char? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) "\n" "\tdq MAKE_LITERAL(T_CHAR, " (string (car const-table)) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((symbol? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL(T_SYMBOL, " (symbol->string (car const-table)) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((string? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tMAKE_LITERAL_STRING \"" (car const-table) "\"\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((null? (car const-table)) 
			  		(string-append "sobNil:" "\n" "\tdq SOB_NIL\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((vector? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tMAKE_LITERAL_VECTOR " (trim-last-comma (get-components-from-pairs (car const-table) table-in-pairs)) "\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((and (list? (car const-table)) (> (length (car const-table)) 1) (list? (cadr (car const-table))))
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL_PAIR (" (find-const-in-pairs (caar const-table) table-in-pairs) ", " (find-const-in-pairs (cadr (car const-table)) table-in-pairs) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((or (list? (car const-table)) (pair? (car const-table))) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL_PAIR (" (find-const-in-pairs (caar const-table) table-in-pairs) ", " (find-const-in-pairs (cdr (car const-table)) table-in-pairs) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  )))

(define trim-last-comma
	(lambda (str)
		;(display "trim-last-comma: ") (display str) (newline) (newline)
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
		;(display "get-components-from-pairs vec: ") (display vec) (newline) 
		;(display "get-components-from-pairs table-in-pairs: ") (display table-in-pairs) (newline) (newline)
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

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR CONSTANT TABLE =============================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR SYMBOL TABLE ======================================================
;=========================================================================================================================================

(define make_symbol_table
	(lambda (exp)
		(cond ((and (list? exp) (null? exp)) exp)
			  ((and (list? exp) (or (equal? (car exp) 'bvar) (equal? (car exp) 'pvar))) (list (cadr exp)))
			  ((and (list? exp) (list? (car exp))) (append (make_symbol_table (car exp)) (make_symbol_table (cdr exp))))
			  ((and (list? exp) (> (length exp) 0) (not (list? (car exp)))) (make_symbol_table (cdr exp))))))

(define symbol_table
	(lambda (input-file)
		(remove-duplicates (make_symbol_table input-file))))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR SYMBOL TABLE ===============================================
;=========================================================================================================================================

;=========================================================================================================================================
;======================================================= FUNCTIONS FOR GLOBAL ENV ========================================================
;=========================================================================================================================================

(define make_global_env
	(lambda (exp)
		(cond ((and (list? exp) (null? exp)) exp)
			  ((and (list? exp) (equal? (car exp) 'fvar)) (list (cadr exp)))
			  ((and (list? exp) (list? (car exp))) (append (make_global_env (car exp)) (make_global_env (cdr exp))))
			  ((and (list? exp) (> (length exp) 0) (not (list? (car exp)))) (make_global_env (cdr exp))))))

(define global_env
	(lambda (input-file)
		(remove-duplicates (make_global_env input-file))))

(define create_global_env_for_assembly
		(lambda (global-env-table)
			(if (null? global-env-table)
				""
				(string-append (symbol->string (car global-env-table)) ":\n" "\tdq SOB_UNDEFINED\n\n" (create_global_env_for_assembly (cdr global-env-table))))))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR GLOBAL ENV =================================================
;=========================================================================================================================================

;=========================================================================================================================================
;======================================================= GENERAL UTILITY FUNCTIONS =======================================================
;=========================================================================================================================================

(define tagged-by?
  (lambda (pe tag)
    (if (not (pair? pe))
	#f
	(eq? tag (car pe)))))

(define remove-duplicates
	(lambda (exp)
		(cond ((null? exp) exp)
			  ((member (car exp) (cdr exp)) (remove-duplicates (cdr exp)))
			  (else (cons (car exp) (remove-duplicates (cdr exp)))))))

;=========================================================================================================================================
;======================================================= END OF GENERAL UTILITY FUNCTIONS ================================================
;=========================================================================================================================================