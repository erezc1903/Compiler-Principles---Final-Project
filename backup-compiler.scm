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
	  (lambda (pe depth const-table global-env)
	  	;(display "const-table in code-gen: ") (display const-table) (newline)
	  	;(display "pe in code-gen: ") (display pe) (newline) (newline)
	      (string-append  
	      	(cond ((and (tagged-by? pe 'const) (not (symbol? (cadr pe)))) (string-append "\t; codegen for const start\n\tmov rax, " (find-const-in-pairs (cadr pe) const-table) "\n\t;code gen for constant end\n"))
	      		   ((and (tagged-by? pe 'const) (symbol? (cadr pe))) (string-append 
																					"\tmov rax," (find-const-in-pairs (cadr pe) const-table) "\n"
																					"\tpush rax\n"
																					"\tpush SymbolTable\n"
																					"\tcall findSymbol\n" 
																					"\tadd rsp, 2*8\n"
																					"\n\t;code gen for symbol end\n"))
			       ((tagged-by? pe 'if3) (handle_if pe depth  const-table global-env))
			       ((tagged-by? pe 'seq) (handle_seq pe depth const-table global-env))
			       ((tagged-by? pe 'or) (handle_or (cadr pe) depth (make-end-label-for-or) const-table global-env))
			       ((tagged-by? pe 'define) (handle_define (cdr pe) depth const-table global-env))
			       ((tagged-by? pe 'applic) (handle_applic pe depth const-table global-env))
			       ((tagged-by? pe 'lambda-simple) (handle_lambda_simple (cadr pe) (caddr pe) depth const-table global-env))
			       ((tagged-by? pe 'tc-applic) (handle_applic pe depth const-table global-env))
			       ((tagged-by? pe 'lambda-opt) (handle_lambda_opt (cadr pe) (cadddr pe) depth const-table global-env))
			       ((tagged-by? pe 'pvar) (handle_pvar pe))
			       ((tagged-by? pe 'bvar) (handle_bvar pe))
			       ((tagged-by? pe 'fvar) (handle_fvar (cadr pe) depth const-table global-env))
			       ((tagged-by? pe 'set) (string-append (code-gen (caddr pe) depth const-table global-env)
			       											"\tmov r10, rax\n"
			       											(code-gen (cadr pe) depth const-table global-env)
			       											"\tmov qword [rax], r10\n"
			       											"\tmov rax, sobVoid\n"))
			       ((tagged-by? pe 'box) (string-append (code-gen (cadr pe) depth const-table global-env)
			      									   "\tmov r10, rax\n"
			      									   "\tmov rdi, 8\n"
			      									   "\tcall malloc\n"
			      									   "\tmov qword [rax], r10\n"))
			       ((tagged-by? pe 'box-get) (string-append (code-gen (cadr pe) depth const-table global-env)
															"\tmov rax, [rax]\n"))
			       ((tagged-by? pe 'box-set) (string-append (code-gen (caddr pe) depth const-table global-env)
			       											"\tmov r10, rax\n"
			       											(code-gen (cadr pe) depth const-table global-env)
			       											"\tmov qword [rax], r10\n"
			       											"\tmov rax, sobVoid\n"))
			       (else ""))))
	      ;(set! num (+ num 1))
	  )

(define compile-scheme-file
	(lambda (in-file out-file)
		(let* ((input (pipeline (append (file->list "scheme_implementations.scm") (file->list in-file))))
			  (out-port (open-output-file out-file 'replace))
			  (index -1)
			  (const-table (const_table input))
			  (const-table-as-list-of-pairs (map (lambda (e) (set! index (+ index 1)) (pairs_of_name_and_object e index)) const-table))
			  (global-env (append primitive-procedures (global_env input)))
			  (global-env-as-pairs (map check_for_special_symbols (map pairs_of_label_and_name global-env))))
			  ;(symbol-table (create-symbol-table input)))

			;(display "global-env-as-pairs: ") (display global-env-as-pairs) (newline) (newline)
			(display "input: ") (display input) (newline) (newline)
			;(display "const-table: ") (display const-table) (newline) (newline)
			;(display "const-table-as-list-of-pairs: ") (display const-table-as-list-of-pairs) (newline) (newline)
			;(display "symbol-table: ") (display (get_symbols const-table)) (newline) (newline)


			(fprintf out-port "%include \"scheme.s\"\n\n") 
			(fprintf out-port "%include \"gcd.s\"\n\n") 
			
			(fprintf out-port "section .bss\n\n") 

			(fprintf out-port "\tSymbolTable: resq  1\n\n")


			
			(fprintf out-port "section .data\n\n") 

			(fprintf out-port (string-append  "size_i:  ; Used to determine the size of the structure\n"
											  "\tstruc node\n"
											  "\t\tsymbol: resq  1\n"
											  "\t\tnext: resq  1\n"
											  "\tendstruc\n\n"))

			;(fprintf out-port "globel_env:\n\n")

			(fprintf out-port (init-primitives primitive-procedures global-env-as-pairs))

			(fprintf out-port (create_global_env_for_assembly global-env-as-pairs))

			(fprintf out-port (create_const_for_assembly const-table const-table-as-list-of-pairs 0))

			;(fprintf out-port (create-string-of-symbols symbol-table))

			(fprintf out-port "\n\nsection .text\n\n")
			(fprintf out-port "\textern exit, printf, scanf, malloc\n\n")
			(fprintf out-port "\tglobal main\n\n")

			(fprintf out-port 
							(string-append "addSymbol:\n"
											"\t\tpush rbp            ; Save the stack\n"
											"\t\tmov rbp, rsp\n"
											"\t\tmov rdi, 16            ; 8 bytes for the symbol and 8 bytes for the next link\n"
											"\t\tcall malloc         ; Call the malloc function - now eax has the address of the allocated memory\n"
											"\t\tmov rbx, [rbp + 3*8]\n"
											"\t\tmov [rax + symbol], rbx    ; Add the element to the node data field\n"
											"\t\tmov qword [rax + next], sobNil   ; Address of the next element is NULL, because it is the last element in the list\n"
											"\t\tmov rbx, [rbp + 2*8]  ; Retrieve the address to the Symbol Table\n"
											"\t\tcmp qword [rbx], 0\n"
											"\t\tje firstElement\n"
											"\t\tmov rbx, [rbx]      ; This parameter was the address of the address\n"
											"\t\t                     ; Now it is the address of the first element, in this case, not null\n"
											"\t\t; If it is not NULL, find the address of the last element\n"
																							
											"next_element:\n\n"
											"\t\tcmp qword [rbx + next], sobNil\n"
											"\t\tje found_last\n"
											"\t\tmov rbx, [rbx + next]\n"
											"\t\tjmp next_element\n"

											"found_last:\n\n"
											"\t\tmov [rbx + next], rax   ; Last element is this one from the newly allocated memory block\n"
											"\t\tjmp doneAdding\n"

											"firstElement:\n\n"
											"\t\tmov [rbx], rax ; Point the address of the first element to the allocated memory\n"

											"doneAdding:\n\n"

											"\t\tmov rsp, rbp\n"
											"\t\tpop rbp\n"
											"\t\tret               ; Return to the caller function and cleaning the stack\n\n"
											
											"findSymbol:\n\n"
											    "\t\tpush rbp\n"
											    "\t\tmov rbp, rsp\n"

											    "\t\tmov r11, [rbp + 2*8]       ;a pointer to the head of the symbol table\n"
											    "\t\tmov r12, [rbp + 3*8]          ; Address of the address of the first element\n"
											    "\t\tmov r11, [r11]\n"
											    ;"\t\tmov r13, r11        ; the head of the symbol table\n"

											    "\tsearch_loop:\n\n"
											    "\t\tmov r13, r11\n"
											   ; "\t\tmov rax, [r12]              ; Address of the first element\n"
											    "\t\tmov r11, [r11 + symbol]\n"
											    ;"\t\tmov r11, [r11]\n"
											    ;"\t\tmov r11, [r11]\n"
											    "\t\tmov r15, [r11]\n"
											    "\t\tDATA r15\n"
											    "\t\tcmp r15, r12\n"
											    "\t\tje done                  ; Don't do anything if the list is empty\n"
											    "\t\tmov r11, r13\n"
											    ;"\t\tmov r11, [r11]\n"
											    "\t\tmov r11, [r11 + next]       ; The next element in the list\n"
											    "\t\tjmp search_loop\n"

											"\tdone:\n\n" 
												"\t\tmov rax, r11\n"
												;"\t\tDATA rax\n"
											    "\t\tmov rsp, rbp\n"
											    "\t\tpop rbp\n"
											    "\t\tret\n"))

			(fprintf out-port "main:\n\n")

			(fprintf out-port "; =============================== PRIMITIVE FUNCTIONS =========================\n")
			(fprintf out-port (creat-primitive-procedures global-env-as-pairs))
			(fprintf out-port "; =============================== PRIMITIVE FUNCTIONS =========================\n")

			(fprintf out-port "start_of_creation_of_symbol_table:\n\n")
			(fprintf out-port (create-symbol-table-for-assembly const-table const-table-as-list-of-pairs))

			(fprintf out-port "\nstart_of_instructions:\n\n")
			(fprintf out-port "\tpush rbp\n") 
			(fprintf out-port "\tmov rbp, rsp\n")
			;(fprintf out-port "\tpush 0\n")

			(for-each (lambda (pe) 
					(fprintf out-port 
						(string-append "\n; start\n" (code-gen pe 0 const-table-as-list-of-pairs global-env-as-pairs)
							"\tmov rax, [rax]\n"
							"\tpush rax\n"
							"\tcall write_sob_if_not_void\n"
							"\tadd rsp, 8\n"
							"\n; end\n"))) input) 

			(fprintf out-port "\tmov rsp, rbp\n")
			(fprintf out-port "\tpop rbp") (newline out-port)
			(fprintf out-port "\tret\n\n") 

			(close-output-port out-port))))



(define creat-primitive-procedures
		(lambda (global-env-as-pairs)
			(string-append
				(handle_pair?) ;V
				(handle_boolean?) ;V
				(handle_integer?) ;V
				(handle_null?) ;V
				(handle_number?) ;V
				(handle_char?) ;V
				(handle_string?) ;V
				(handle_symbol?)
				(handle_vector?) ;V
				(handle_not) ;V
				(handle_rational?) ;V
				(handle_zero?) ;V
				(handle_car) ;V
				(handle_cdr) ;V
				(handle_cons) ;V
				(handle_numerator) ;V
				(handle_denominator) ;V
				(handle_integer->char) ;V
				(handle_char->integer) ;V
				(handle_plus) ;V
				(handle_greater_than) ;V
				(handle_less_than) ;V
				(handle_equal) ;V
				(handle_remainder) ;V
				(handle_string_length); V
				(handle_string_ref) ;V
				(handle_string_set) ;V
				(handle_make_string) ;V
				(handle_multiply) ;V
				(handle_subtract) ;V
				(handle_make_vector) ;V
				(handle_procedure?) ;V
				(handle_apply) ;V 
				(handle_vector_length) ;V
				(handle_vector_ref) ;V
				(handle_vector) ;V
				(handle_vector_set) ;V
				(handle_set_car) ;V
				(handle_set_cdr) ;V
				(handle_divide) ;V
				(handle_symbol->string)
				(handle_string->symbol)
				(handle_eq?)
				 "")))




;=========================================================================================================================================
;======================================================= FUNCTIONS FOR APP EXPRESSION ====================================================
;=========================================================================================================================================

(define handle_applic
		(lambda (app-exp depth const-table global-env)
			;(display "handle_applic app: ") (display app-exp) (newline)
			;(display "handle_applic depth: ") (display depth) (newline) (newline)
			
			(let ((app (cadr app-exp))
				  (args (caddr app-exp))
				  (not-a-closure-label (make-not-a-closure-label))
				  (done-closure-label (make-done-closure-label)))
				  ;(display "app: ") (display app) (newline)
				  ;(if (equal? (cadr app) 'apply) (handle_apply app-exp depth const-table global-env)
					  	(string-append "; start of applic of lambda-simple code: \n\n"
					  					"\tmov rax, sobNil\n"
					  					"\tpush rax\n"
										(push-args (reverse args) (length args) depth const-table global-env)
										"\tpush " (number->string (length args)) "\n"
										(code-gen app depth const-table global-env)
										"\tmov r10, [rax]\n" 
										"\tmov rcx, r10\n"
										"\tTYPE rcx\n"
										"\tcmp rcx, T_CLOSURE\n"
										"\tjne " not-a-closure-label "\n"
										"\tmov rbx, r10\n"
										"\tCLOSURE_ENV rbx\n"
										"\tpush rbx\n"
										"\tCLOSURE_CODE r10\n"
										"\tcall r10\n"
										"\tadd rsp, 8*1\n"
										"\tjmp " done-closure-label "\n"
										not-a-closure-label ":\n\n"
										"\tmov rax, sobVoid\n"
										done-closure-label ":\n\n"
										"\tpop r15\n"
										"\tshl r15, 3\n"
										"\tadd rsp, r15\n\n"
										"\tadd rsp, 8\n"
										"; end of applic of lambda-simple code: \n\n"))
					))

; we assume that the arg list comes reversed
(define push-args 
		(lambda (args numberOfArgs depth const-table global-env)
			;(display "push-args args: ") (display args) (newline)
			;(display "push-args numberOfArgs: ") (display numberOfArgs) (newline) (newline)
			(if (= numberOfArgs 0)
				"\n"
				(string-append (code-gen (car args) depth const-table global-env)
								"\tpush rax\n"
								(push-args (cdr args) (length (cdr args)) depth const-table global-env)))))

(define make-not-a-closure-label
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "not_a_closure" (number->string num)))))

(define make-done-closure-label
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "done_closure" (number->string num)))))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR APP EXPRESSION =============================================
;=========================================================================================================================================

;=========================================================================================================================================
;======================================================= FUNCTIONS FOR FVAR+PVAR+BVAR EXPRESSION =========================================
;=========================================================================================================================================

(define handle_fvar
		(lambda (var depth const-table global-env)
			(string-append  "\tmov rax, " (find-var-in-global-env var global-env) "\n")))


(define handle_pvar
  (lambda (pe)
    (let ((min (caddr pe)))
      (string-append "\tmov rax, qword [rbp + (4+" (number->string min)")*8]\n"))))


(define handle_bvar
  (lambda (pe)
    (let ((major (caddr pe))
	  	  (minor (cadddr pe)))
	      (string-append "\tmov rax, qword [rbp + 2*8]\n"
	      				 "\tmov rax, qword [rax + "(number->string major) "*8]\n"
	      				 "\tmov rax, qword [rax + "(number->string minor) "*8]\n"))))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR FVAR+PVAR+BVAR EXPRESSION ==================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR LAMBDA SIMPLE EXPRESSION ==========================================
;=========================================================================================================================================


(define handle_lambda_simple
		(lambda (params body depth const-table global-env)
			;(display "handle_lambda_simple params: ") (display params) (newline)
			;(display "handle_lambda_simple body: ") (display body) (newline)
			;(display "handle_lambda_simple depth: ") (display depth) (newline)
			(let* ((body-label (make-body-label-for-lambda-simple))
				  (copy-args-label (make-copy-args-label-for-lambda-simple))
				  (copy-env-label (make-copy-env-label-for-lambda-simple))
				  (no-args-label (make-no-args-label-for-lambda-simple))
				  (done-copy-args (make-done-copying-args-label-for-lambda-simple))
				  (make-closure-label (make-make-closure-label-for-lambda-simple))
				  (end-label (make-end-label-for-lambda-simple))
				  (done-copy-env (make-done-copying-env-label-for-lambda-simple))
				  (bad-args-label (make-bad-arg-count-label-for-lambda-simple))
				  (extended-env
				  	 (string-append 
				  	 			"\txor rax, rax\n"
				 				"\tmov rdi, qword [rbp + 3*8]\n"
				 				"\tcall malloc\n"
				 				"\tmov rdx, rax" "; rdx hold a pointer to store the params\n"
				 				;"after1:\n"
				 				"\tpush rdx\n"
				 				"\txor rax, rax\n"
				 				"\tmov rdi, " (number->string (* 8 (+ depth 1))) "\n"
				 				"\tcall malloc\n"
				 				"\tmov rbx, rax\n" "; rbx hold a pointer to store the previous environment\n"
				 				;"after2:\n"
				 				"\tpop rdx\n"
				 				"\tpush rdx\n"
				 				"\tpush rbx\n"
				 				"\txor rax, rax\n"
				 				"\tmov rdi, 16\n"
				 				"\tcall malloc" "; rax now hold a pointer to the target closure\n"
				 				;"after3:\n"
				 				"\tpop rbx\n"
				 				"\tpop rdx\n\n"
				 				
				 				"\tmov rcx, 0\n"
				 				;"\tcmp rcx, [rbp + 3*8] " "; check if there are arguments in the lambda\n"
				 				;"\tje " no-args-label "\n"
				 				copy-args-label":\n"
				 				;"\tmov r12, qword [rbp + 3*8]\n"
				 				"\tcmp rcx, qword [rbp + 3*8] " "; check if we are done copying the args in the lambda\n"
				 				"\tje " done-copy-args "\n"
				 				"\tmov r9, qword [rbp + 8*(4 + rcx)]\n"
				 				"\tmov [rdx + rcx*8], r9\n"
				 				"\tinc rcx\n"
				 				"\tjmp " copy-args-label "\n\n"

				 				;no-args-label":\n"
				 				;"\tmov qword [rdx], 0\n"

								done-copy-args":\n"
				 				"\tmov [rbx], rdx\n"
				 				"\tmov r10, 0\n"
				 				"\tmov r15, 1\n\n"

				 				copy-env-label":\n"
				 				"\tcmp r10, " (number->string (- depth 1)) "\n"
				 				"\tje " done-copy-env "\n" 
				 				"\tmov r12, [rbp + 8*2]\n"
				 				"\tmov r12, [r12 + 8*r10]\n"
				 				"\tmov [rbx + 8*r15], r12\n"
				 				"\tinc r10\n"
				 				"\tinc r15\n"
				 				"\tjmp " copy-env-label "\n"
				 				done-copy-env":\n\n"
				 				"\tmov qword [rbx + 8*r15], 0\n"))
				  (new-env (if (= depth 0) (string-append "\tmov rbx, 0\n"
				  										  "\tmov rdi, 16\n"
				 										  "\tcall malloc" "; rax now hold a pointer to the target closure\n") extended-env)))

				  		(string-append 
				  				"; start of creating a closure of lambda-simple " (number->string depth) "\n\n" 
				  				new-env

				 				make-closure-label":\n\n"

				 				"\tMAKE_LITERAL_CLOSURE rax, rbx, " body-label "\n"
				 				"\tjmp " end-label "\n\n"

				 				body-label ":\n"
				 				"\tpush rbp\n"
				 				"\tmov rbp, rsp\n"
				 				"\tmov r10, qword [rbp +3*8]\n"
				 				"\tcmp r10, " (number->string (length params)) "\n"
				 				"\tjne " bad-args-label "\n"
							    (code-gen body (+ depth 1) const-table global-env)
							    ;"\tmov rsp, rbp\n"
							    "\tmov rsp, rbp\n"
								"\tpop rbp\n"
								"\tret\n\n"

								bad-args-label":\n"
								"\tmov rax, sobVoid\n"
								"\tmov rsp, rbp\n"
								"\tpop rbp\n"
								"\tret\n\n"
								end-label":\n"
								;"\tmov rax, [rax] ; rax now hold the closure object \n\n"

						"; end of creating a closure of lambda-simple " (number->string depth) "\n\n"))))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR LAMBDA SIMPLE EXPRESSION ===================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR LAMBDA OPT EXPRESSION =============================================
;=========================================================================================================================================


(define handle_lambda_opt
		(lambda (params body depth const-table global-env)
			;(display "handle_lambda_simple params: ") (display params) (newline)
			;(display "handle_lambda_simple body: ") (display body) (newline)
			;(display "handle_lambda_simple depth: ") (display depth) (newline)
			(let* ((body-label (make-body-label-for-lambda-simple))
				  (copy-args-label (make-copy-args-label-for-lambda-simple))
				  (copy-env-label (make-copy-env-label-for-lambda-simple))
				  (no-args-label (make-no-args-label-for-lambda-simple))
				  (done-copy-args (make-done-copying-args-label-for-lambda-simple))
				  (make-closure-label (make-make-closure-label-for-lambda-simple))
				  (end-label (make-end-label-for-lambda-simple))
				  (done-copy-env (make-done-copying-env-label-for-lambda-simple))
				  (bad-args-label (make-bad-arg-count-label-for-lambda-simple))
				  (opt-args-loop (make-opt-args-loop-label-for-lambda-simple))
				  (opt-args-loop-end (make-opt-args-loop-end-label-for-lambda-simple))
				  (extended-env
				  	 (string-append 
				  	 			"\txor rax, rax\n"
				 				"\tmov rdi, qword [rbp + 3*8]\n"
				 				"\tcall malloc\n"
				 				"\tmov rdx, rax" "; rdx hold a pointer to store the params\n"
				 				;"after1:\n"
				 				"\tpush rdx\n"
				 				"\txor rax, rax\n"
				 				"\tmov rdi, " (number->string (* 8 (+ depth 1))) "\n"
				 				"\tcall malloc\n"
				 				"\tmov rbx, rax\n" "; rbx hold a pointer to store the previous environment\n"
				 				;"after2:\n"
				 				"\tpop rdx\n"
				 				"\tpush rdx\n"
				 				"\tpush rbx\n"
				 				"\txor rax, rax\n"
				 				"\tmov rdi, 16\n"
				 				"\tcall malloc" "; rax now hold a pointer to the target closure\n"
				 				;"after3:\n"
				 				"\tpop rbx\n"
				 				"\tpop rdx\n\n"
				 				
				 				"\tmov rcx, 0\n"
				 				;"\tcmp rcx, [rbp + 3*8] " "; check if there are arguments in the lambda\n"
				 				;"\tje " no-args-label "\n"
				 				copy-args-label":\n"
				 				;"\tmov r12, qword [rbp + 3*8]\n"
				 				"\tcmp rcx, qword [rbp + 3*8] " "; check if we are done copying the args in the lambda\n"
				 				"\tje " done-copy-args "\n"
				 				"\tmov r9, qword [rbp + 8*(4 + rcx)]\n"
				 				"\tmov [rdx + rcx*8], r9\n"
				 				"\tinc rcx\n"
				 				"\tjmp " copy-args-label "\n\n"

				 				;no-args-label":\n"
				 				;"\tmov qword [rdx], 0\n"

								done-copy-args":\n"
				 				"\tmov [rbx], rdx\n"
				 				"\tmov r10, 0\n"
				 				"\tmov r15, 1\n\n"

				 				copy-env-label":\n"
				 				"\tcmp r10, " (number->string (- depth 1)) "\n"
				 				"\tje " done-copy-env "\n" 
				 				"\tmov r12, [rbp + 8*2]\n"
				 				"\tmov r12, [r12 + 8*r10]\n"
				 				"\tmov [rbx + 8*r15], r12\n"
				 				"\tinc r10\n"
				 				"\tinc r15\n"
				 				"\tjmp " copy-env-label "\n"
				 				done-copy-env":\n\n"
				 				"\tmov qword [rbx + 8*r15], 0\n"))
				  
				  (new-env (if (= depth 0) (string-append "\tmov rbx, 0\n"
				  										  "\tmov rdi, 16\n"
				 										  "\tcall malloc" "; rax now hold a pointer to the target closure\n") extended-env)))

				  		(string-append 
				  				"; start of creating a closure of lambda-simple " (number->string depth) "\n\n" 
				  				new-env

				 				make-closure-label":\n\n"

				 				"\tMAKE_LITERAL_CLOSURE rax, rbx, " body-label "\n"
				 				"\tjmp " end-label "\n\n"

				 				body-label ":\n"
				 				"\tpush rbp\n"
				 				"\tmov rbp, rsp\n"
				 				"\tmov r10, qword [rbp +3*8]\n"
				 				"\tcmp r10, " (number->string (length params)) "\n"
				 				"\tjl " bad-args-label "\n"

			 				   
			 				   "\tmov r15, " (number->string (- (length params) 1)) "\n"
			 				   "\tmov r14, qword [rbp + 3*8]\n"
			 				   "\tdec r14\n"
			 				   "\tmov r13, sobNil\n\n"


			 				   opt-args-loop ":\n\n"
			 				   "\tcmp r14, r15\n"
			 				   "\tje " opt-args-loop-end "\n"

			 				   "\tmov rdi, 8\n"
			 				   "\tcall malloc\n"
			 				   "\tmov r8, qword [rbp + 4*8 + r14*8]\n"
			 				   "\tmov r12, r8\n"
			 				   "\tsub r12, start_of_data\n"
			 				   "\tshl r12, 30\n"
			 				   "\tmov r9, r13\n"
			 				   "\tsub r9, start_of_data\n"
			 				   "\tor r12, r9\n"
			 				   "\tshl r12, 4\n"
			 				   "\tor r12, T_PAIR\n"
			 				   "\tmov  qword [rax], r12\n"
			 				   "\tmov r13, rax\n"
			 				   "\tdec r14\n"
			 				   "\tjmp " opt-args-loop "\n"

			 				   opt-args-loop-end ":\n\n"
			 				   "\tmov qword [rbp + 4*8 + (r15 + 1)*8], r13\n"
								
							   (code-gen body (+ depth 1) const-table global-env)

							   "\tmov rsp, rbp\n"
							   "\tpop rbp\n"
							   "\tret\n\n"

							   bad-args-label":\n"
							   "\tmov rax, sobVoid\n"
							   "\tmov rsp, rbp\n"
							   "\tpop rbp\n"
							   "\tret\n\n"
							   end-label":\n"

						"; end of creating a closure of lambda-simple " (number->string depth) "\n\n"))))


(define make-body-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "bodyOfLambda" (number->string num)))))

(define make-copy-args-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "copy_args_loop" (number->string num)))))

(define make-no-args-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "no_args" (number->string num)))))

(define make-copy-env-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "copy_env_loop" (number->string num)))))

(define make-make-closure-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "make_closure" (number->string num)))))


(define make-end-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "endLabel" (number->string num)))))

(define make-done-copying-args-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "done_copy_args" (number->string num)))))

(define make-done-copying-env-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "done_copy_env" (number->string num)))))

(define make-bad-arg-count-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "bad_arg_count" (number->string num)))))

(define make-opt-args-loop-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "opt_args_loop" (number->string num)))))

(define make-opt-args-loop-end-label-for-lambda-simple
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "opt_args_loop_end" (number->string num)))))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR LAMBDA OPT EXPRESSION ======================================
;=========================================================================================================================================


(define applic-prolog
		(lambda (app-label end-app-label)
			(string-append "\tmov rbp, rsp\n"
						   "\tmov rdi, 16\n"
						   "\tcall malloc\n"
						   "\tmov rbx, 1\n"
						   "\tMAKE_LITERAL_CLOSURE rax, rbx, " app-label "\n"
						   "\tjmp " end-app-label "\n\n")))



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR APPLY EXPRESSION ==================================================
;=========================================================================================================================================


(define handle_apply
		(lambda ()
			(let* ((count-args-label (make-count-args-label))
				  (end-count-args-label (make-end-count-args-label))
				  (copy-args-label (make-copy-args-label))
				  (done-copy-args-label (make-done-copy-args-label))
				  (bad-input-label (make-bad-input-label))
				  (end-label (make-end-label)))

			(string-append (applic-prolog "apply_code" "end_apply_code")
			

				"\napply_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 2\n"
				"\tjne " bad-input-label "\n"
				"\tmov r10, qword [rbp + 8*4]\n"
				"\tmov r10, [r10]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_CLOSURE\n"
				"\tjne " bad-input-label"\n"
				"\tmov r10, qword [rbp + 8*5]\n"
				"\tmov r10, [r10]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_PAIR\n"
				"\tjne " bad-input-label "\n"

				"; calculate list length\n"
				"\tmov r10, qword [rbp + 5*8]\n"
				"\tmov r13, 0\n"

				count-args-label":\n\n"
				"\tmov r10, [r10] ; r10 holds the head of the list\n"
				"\tmov r12, r10\n"
				"\tTYPE r12\n"
				"\tcmp r12, T_NIL\n"
				"\tje " end-count-args-label "\n"
				"\tDATA_LOWER r10\n"
				"\tadd r10, start_of_data\n"
				"\tinc r13\n"
				"\tjmp " count-args-label "\n"

				end-count-args-label":\n\n"

				"\tmov r10, qword [rbp + 8*4] ; holds a pointer to f on the stack\n"
				"\tmov r10, [r10] ; holds the actual f on the stack\n"
				"\tlea r12, [rbp + 8*5] ; holds a pointer to the list of arguments on the stack\n"
				"; r13 holds the number of arguments. calculated before\n"
				"\tmov r14, r13 ; r14 is the new number of arguments\n"
				"\tlea r15, [rbp + 2*8]\n"
				"\tshl r13, 3\n"
				"\tsub r15, r13 ; r15 holds the address of where to we should copy the old rbp\n"
				"\tmov r11, r15 ; r11 holds a backup of the address of where to we should copy the old rbp\n"
				"\tmov r8, qword [rbp + 1*8] ; r8 hold the return address\n"
				"\tmov rbx, qword [rbp]\n"
				"\tmov qword [r15], rbx ; r15 hold the old rbp\n"
				"\tadd r15, 8\n"
				"\tmov qword [r15], r8\n"
				"\tCLOSURE_ENV rcx ; rcx now hold the environment of f\n"
				"\tadd r15, 8\n"
				"\tmov qword [r15], rcx\n"
				"\tadd r15, 8\n"
				"\tmov qword [r15], r14\n"
				"\tadd r15, 8\n"

				"\tmov r12, [r12]\n"

				copy-args-label":\n\n"

				"\tmov rdx, r12\n"
				"\tmov rdx, [rdx]\n"
				"\tTYPE rdx\n"
				"\tcmp rdx, T_NIL\n"
				"\tje " done-copy-args-label "\n"

				"\tmov rdx, r12\n"
				"\tmov rdx, [rdx]\n"
				"\tDATA_UPPER rdx\n"
				"\tadd rdx, start_of_data\n"
				"\tmov qword [r15], rdx\n"
				"\tadd r15, 8\n"
				"\tmov rdx, r12\n"
				"\tmov rdx, [rdx]\n"
				"\tDATA_LOWER rdx\n"
				"\tadd rdx, start_of_data\n"
				"\tmov r12, rdx\n"
				"\tjmp " copy-args-label "\n"

				done-copy-args-label":\n\n"
				"\tjmp " end-label "\n"
				

				bad-input-label":\n\n"
				"\tmov rax, sobVoid\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				end-label":\n"
				"\tmov rsp, r11\n"
				"\tpop rbp\n"
				"\tmov rdx, r10\n"
				"\tCLOSURE_CODE rdx\n"
				"\tjmp rdx\n"


				"end_apply_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [apply], rax\n\n"))))

(define make-count-args-label
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "countArgs" (number->string num)))))

(define make-end-count-args-label
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "endCountArgs" (number->string num)))))

(define make-copy-args-label
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "copyArgs" (number->string num)))))

(define make-done-copy-args-label
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "doneCopyArgs" (number->string num)))))

(define make-bad-input-label
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "badInput" (number->string num)))))

(define make-end-label
	(let ((num 100))
			(lambda ()
				(set! num (+ num 1))
				(string-append "end" (number->string num)))))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR APPLY EXPRESSION ===========================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR APPEND EXPRESSION =================================================
;=========================================================================================================================================


;(define handle_append
;		(lambda ()
;			(string-append (applic-prolog "append_code" "end_append_code")

;				"\nappend_code:\n"
;				"\tpush rbp\n"
;				"\tmov rbp, rsp\n"
				
;				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
;				".checkIfArgsArePairs:\n\n"
;				"\tcmp rcx, qword [rbp + 8*3]\n"
;				"\tje .create_new_list\n"
;				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
;				"\tmov rbx, [rbx]\n"
;				"\tTYPE rbx\n"
;				"\tcmp rbx, T_PAIR\n"
;				"\tjne .badArgs\n"
;				"\tinc rcx\n"
;				"\tjmp .checkIfArgsArePairs\n"


;				;"\tmov r15, sobNil ; r9 hold the last pair - '() of the new list\n"

;				;".create_new_list_loop:\n\n"
;				;"\tcmp r8, 0\n"
;				;"\tje .done_create_new_list_loop\n"
;				;;"\tmov rdi, 8\n"
;				;;"\tcall malloc\n"
;				;;"\tmov r10, rax ; r10 hold the address of the new pair\n"
;				;;"\tmov r11, qword [rbp + (4 + r8)*8] ; r11 hold the current pair which will be the car of the new pair\n"
;				;;"\tmov r11, [r11]\n"
;				;"\tmov rdi, 8\n"
;				;"\tcall malloc\n"
;				;"\tmov r13, rax\n"
;				;"\tmov r14, [rbp + (4 + r8)*8] ; r14 hold the pointer to the car of the new pair\n"
;				;"\tsub r14, start_of_data\n"
;				;"\tshl r14, 30\n"
;				;"\tsub r15, start_of_data\n"
;				;"\tor r14, r15\n"
;				;"\tshl r14, 4\n"
;				;"\tor r14, T_PAIR\n"
;				;"\tmov r15, r14 ; r15 hold the accumulated pair\n"
;				;"\tmov [r13], r14\n"
;				;;"\tMAKE_MALLOC_LITERAL_PAIR r10, r11, r9\n"
;				;;"\tmov r9, r10 ; r9 hold the last pair of the new list\n"
;				;"\tdec r8\n"
;				;"\tjmp .create_new_list_loop\n"


;				;"\tmov r15, " (number->string (- (length params) 1)) "\n"
;			    ;"\tmov r14, qword [rbp + 3*8]\n"
;			    ;"\tdec r14\n"
;			    ".create_new_list:\n\n"

;				"\tmov r8, qword [rbp + 8*3]\n"
;				;"\tmov r11, qword [rbp + 8*3]\n"
;				"\tsub r8, 1\n"
;			    "\tmov r13, sobNil\n\n"


;			    ".create_new_list_loop:\n\n"
;			    "\tcmp r8, -1\n"
;			    "\tje .done_create_new_list_loop\n"

;			    "\tmov rdi, 8\n"
;			    "\tcall malloc\n"
;			    "\tmov r10, qword [rbp + 4*8 + r8*8]\n"
;			    "\tmov r12, r10\n"
;			    "\tsub r12, start_of_data\n"
;			    "\tshl r12, 30\n"
;			    "\tmov r9, r13\n"
;			    "\tsub r9, start_of_data\n"
;			    "\tor r12, r9\n"
;			    "\tshl r12, 4\n"
;			    "\tor r12, T_PAIR\n"
;			    "\tmov  qword [rax], r12\n"
;			    "\tmov r13, rax\n"
;			    "\tdec r8\n"
;			    "\tjmp .create_new_list_loop\n"

;				".done_create_new_list_loop:\n\n"
;				"\tmov rsp, rbp\n" 
;				"\tpop rbp\n"
;				"\tret\n\n"
;				;"\tmov rdi, 8\n"
;				;"\tcall malloc\n"
;				;"\tmov qword [rax], r9\n"
;				;"\tmov rax, r9\n"
;				;"\tjmp .done\n"

;				".badArgs:\n"
;				"\tmov rax, sobVoid\n"

;				;".done:\n"
;				;"\tmov rsp, rbp\n" 
;				;"\tpop rbp\n"
;				;"\tret\n\n"

;				"end_append_code:\n"
;				"\tmov rax, [rax]\n"
;				"\tmov qword [append], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR APPEND EXPRESSION ==========================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR VECTOR-LENGTH EXPRESSION ==========================================
;=========================================================================================================================================

(define handle_vector_length
		(lambda () 

			(string-append (applic-prolog "vector_length_code" "end_vector_length_code")

				"\nvector_length_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notAVector\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_VECTOR\n"
				"\tjne .notAVector\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tVECTOR_LENGTH r10\n"
				"\tshl r10, 4\n"
				"\tor r10, T_INTEGER\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r10\n"
				"\tjmp .done\n\n"
				".notAVector:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_vector_length_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [vectorLength], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR VECTOR-LENGTH EXPRESSION ===================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR VECTOR-SET EXPRESSION =============================================
;=========================================================================================================================================

(define handle_vector_set
		(lambda () 

			(string-append (applic-prolog "vector_set_code" "end_vector_set_code")

				"\nvector_set_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 3\n"
				"\tjl .badArgs\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_VECTOR\n"
				"\tjne .badArgs\n"
				"\tmov rcx, qword [rbp + 8*5]\n"
				"\tmov r10, [rcx]\n"
				"\tTYPE r10\n"
				"\tcmp r10, T_INTEGER\n"
				"\tjne .badArgs\n"
				"\tmov r13, qword [rbp + 8*4] ; rbx holds a pointer to the vector\n"
				"\tmov r13, [r13] ; hold the actual vector\n"
				"\tmov r9, qword [rbp + 8*5]\n"
				"\tmov r10, [r9] ; r10 hold k - the position in the vector\n"
				"\tDATA r10\n"
				"\tmov r12, qword [rbp + 8*6] ; r12 holds a pointer to the object to replace the k-th element of the vector \n"
				"\tVECTOR_ELEMENTS r13\n"
				"\tmov qword [r13 + r10*8], r12\n"
				"\tmov rax, sobVoid\n"
				"\tjmp .done\n\n"
				".badArgs:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_vector_set_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [vectorSet], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR VECTOR-SET EXPRESSION ======================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR VECTOR-REF EXPRESSION =============================================
;=========================================================================================================================================

(define handle_vector_ref
		(lambda () 

			(string-append (applic-prolog "vector_ref_code" "end_vector_ref_code")

				"\nvector_ref_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 2\n"
				"\tjne .badArgs\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_VECTOR\n"
				"\tjne .badArgs\n"
				"\tmov rcx, qword [rbp + 8*5]\n"
				"\tmov r10, [rcx]\n"
				"\tTYPE r10\n"
				"\tcmp r10, T_INTEGER\n"
				"\tjne .badArgs\n"
				"\tmov rbx, qword [rbp + 8*4] ; a pointer to the vector\n"
				"\tmov rbx, [rbx]\n"
				"\tmov r9, qword [rbp + 8*5]\n"
				"\tmov r10, [r9] ; the position in the vector\n"
				"\tDATA r10\n"
				"\tVECTOR_ELEMENTS rbx\n"
				"\tmov rax, [rbx + r10*8]\n"
				"\tjmp .done\n\n"
				".badArgs:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_vector_ref_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [vectorRef], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR VECTOR-REF EXPRESSION ======================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR VECTOR EXPRESSION =================================================
;=========================================================================================================================================

(define handle_vector
		(lambda ()
			(let* ((body-label (make-body-label-for-vector))
				  (end-label (make-end-label-for-vector))		  
				  (bad-input-label (make-bad-input-for-vector))
				  (insert-args-loop-label (make-insert-args-loop-input-for-vector))
				  (insert-args-loop-end-label (make-insert-args-loop-input-end-for-vector)))

				  

				  (string-append (applic-prolog "vector_code" "end_vector_code")

				  		"\tvector_code:\n\n"
						"\tpush rbp\n"
						"\tmov rbp, rsp\n"

						; Initializing the first qword of the vector
						"\tmov r11, qword [rbp + 3*8] ; length of the vector\n"
						"\tinc r11\n"
						"\tshl r11, 3\n"
						"\tmov rdi, r11\n"
						"\tcall malloc\n"
						"\tmov r11, qword [rbp + 3*8] ; length of the vector\n"
						"\tmov qword [rax], r11\n"
						"\tshl qword [rax], 30\n"
						"\tlea r11, [rax + 1*8]\n"
						"\tsub r11, start_of_data\n"
						"\tor qword [rax], r11\n"
						"\tshl qword [rax], TYPE_BITS\n"
						"\tor qword [rax], T_VECTOR\n"


						; Loop over the vector length and put in its elements the address of the second argument.
						"\tmov r15, qword [rbp + 3*8] ; length of the vector\n"
						"\tmov r14, 0\n"
						;"\tmov r12, qword [rbp + 4*8] ; first element of the vector\n"


						insert-args-loop-label":\n\n"
						"\tcmp r14, r15\n"
						"\tje " insert-args-loop-end-label "\n"
						"\tmov r12, qword [rbp + (4 + r14)*8]\n"
						"\tmov qword [rax + 1*8 + r14*8], r12\n"
						"\tinc r14\n"
						"\tjmp " insert-args-loop-label "\n"

						insert-args-loop-end-label ":\n\n"
						"\tjmp " end-label "\n"

						end-label ":\n\n"
						"\tmov rsp, rbp\n"
						"\tpop rbp\n"
						"\tret\n"


						"end_vector_code:\n"
						"\tmov rax, [rax]\n"
						"\tmov qword [vector], rax\n\n"))))


(define make-body-label-for-vector
	(let ((num 400))
			(lambda ()
				(set! num (+ num 1))
				(string-append "bodyOfVector" (number->string num)))))

(define make-end-label-for-vector
	(let ((num 400))
			(lambda ()
				(set! num (+ num 1))
				(string-append "endLabel" (number->string num)))))


(define make-bad-input-for-vector
	(let ((num 400))
			(lambda ()
				(set! num (+ num 1))
				(string-append "badInputForVector" (number->string num)))))

(define make-insert-args-loop-input-for-vector
	(let ((num 400))
			(lambda ()
				(set! num (+ num 1))
				(string-append "insertArgSLoopForVector" (number->string num)))))

(define make-insert-args-loop-input-end-for-vector
	(let ((num 400))
			(lambda ()
				(set! num (+ num 1))
				(string-append "insertArgsLoopEndForVector" (number->string num)))))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR VECTOR EXPRESSION ==========================================
;=========================================================================================================================================




;=========================================================================================================================================
;======================================================= FUNCTIONS FOR MAKE-VECTOR EXPRESSION ============================================
;=========================================================================================================================================

(define handle_make_vector
		(lambda ()
			(let* ((body-label (make-body-label-for-make-vector))
				  (no-args-label (make-no-args-label-for-make-vector))
				  (end-label (make-end-label-for-make-vector))
				  (one-arg-label (make-one-arg-for-make-vector))
				  (two-arg-label (make-two-arg-for-make-vector))				  
				  (bad-input-label (make-bad-input-for-make-vector))
				  (one-arg-loop-label (make-one-arg-loop-input-for-make-vector))
				  (one-arg-loop-end-label (make-one-arg-loop-input-end-for-make-vector))
				  (two-arg-loop-label (make-two-arg-loop-input-for-make-vector))
				  (two-arg-loop-end-label (make-two-arg-loop-input-end-for-make-vector)))

				  

				  (string-append (applic-prolog "make_vector_code" "end_make_vector_code")

				  		"\tmake_vector_code:\n\n"
						"\tpush rbp\n"
						"\tmov rbp, rsp\n"


						"\tcmp qword [rbp + 3*8], 1\n"
						"\tje " one-arg-label "\n"

						"\tcmp qword [rbp + 3*8], 2\n"
						"\tje " two-arg-label "\n"
						"\tjmp " bad-input-label "\n"

						one-arg-label ":\n"

						"\tmov rcx, qword [rbp + 4*8]\n"
						"\tmov r10, [rcx]\n"
						"\tTYPE r10\n"
						"\tcmp r10, T_INTEGER\n"
						"\tjne "bad-input-label "\n"

						; Put code for #(0 0 0 0 0) here
						; Initializing the runtime constant zero and putting its address into r12
						"\tmov rdi, 8\n"
						"\tcall malloc\n"
						"\tmov qword [rax], 0\n"
						"\tshl qword [rax], TYPE_BITS\n"
						"\tor qword [rax], T_INTEGER\n"
						"\tmov r12, rax ; r12 now holds a pointer to a runtime constant zero\n"


						; Initializing the first qword of the vector
						"\tmov r8, qword [rbp + 4*8] ; r8 holds a pointer to the vector length \n"
						"\tmov r13, [r8]\n"
						"\tDATA r13\n"
						"\tinc r13\n"
						"\tshl r13, 3\n"
						"\tmov rdi, r13\n"
						"\tcall malloc\n"
						"\tmov r13, [r8]\n"
						"\tDATA r13 \n"
						"\tmov qword [rax], r13\n"
						"\tshl qword [rax], 30\n"
						"\tlea r13, [rax + 1*8]\n"
						"\tsub r13, start_of_data\n"
						"\tor qword [rax], r13\n"
						"\tshl qword [rax], TYPE_BITS\n"
						"\tor qword [rax], T_VECTOR\n"


						; Loop over the vector length and initialize its elements to zero.
						"\tmov r15, [r8]\n"
						"\tDATA r15\n"
						"\tmov r14, 0\n"


						one-arg-loop-label ":\n\n"
						"\tcmp r14, r15\n"
						"\tje " one-arg-loop-end-label "\n"

						"\tmov qword [rax + 1*8 + r14*8], r12\n"
						"\tinc r14\n"
						"\tjmp " one-arg-loop-label "\n"

						one-arg-loop-end-label ":\n\n"
						"\tjmp " end-label "\n"


						two-arg-label":\n\n"

						"\tmov r8, qword [rbp + 4*8]\n"
						"\tmov r10, [r8]\n"
						"\tTYPE r10\n"
						"\tcmp r10, T_INTEGER\n"
						"\tjne "bad-input-label "\n"
						"\tmov r11, [r8]\n"
						"\tDATA r11\n"
						"\tcmp r11, 0\n"
						"\tjl " bad-input-label "\n"


						; Put code for #(4 4 4 4 4) here
						; Setting r12 to the second argument
						"\tmov r8, qword [rbp + 4*8] ; length of the vector\n"
						"\tmov r12, qword [rbp + 5*8] ; elements of the vector\n"


						; Initializing the first qword of the vector
						"\tmov r11, [r8]\n"
						"\tDATA r11\n"
						"\tinc r11\n"
						"\tshl r11, 3\n"
						"\tmov rdi, r11\n"
						"\tcall malloc\n"
						"\tmov r11, [r8]\n"
						"\tDATA r11\n"
						"\tmov qword [rax], r11\n"
						"\tshl qword [rax], 30\n"
						"\tlea r11, [rax + 1*8]\n"
						"\tsub r11, start_of_data\n"
						"\tor qword [rax], r11\n"
						"\tshl qword [rax], TYPE_BITS\n"
						"\tor qword [rax], T_VECTOR\n"


						; Loop over the vector length and put in its elements the address of the second argument.
						"\tmov r15, [r8]\n"
						"\tDATA r15\n"
						"\tmov r14, 0\n"


						two-arg-loop-label":\n\n"
						"\tcmp r14, r15\n"
						"\tje " two-arg-loop-end-label "\n"

						"\tmov qword [rax + 1*8 + r14*8], r12\n"
						"\tinc r14\n"
						"\tjmp " two-arg-loop-label "\n"

						two-arg-loop-end-label ":\n\n"
						"\tjmp " end-label "\n"


						bad-input-label":\n\n"
						"\tmov rax, sobVoid\n"
						
						end-label ":\n\n"
						"\tmov rsp, rbp\n"
						"\tpop rbp\n"
						"\tret\n"


						"end_make_vector_code:\n"
						"\tmov rax, [rax]\n"
						"\tmov qword [makeVector], rax\n\n"))))


(define make-body-label-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "bodyOfMakeVector" (number->string num)))))

(define make-no-args-label-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "no_args" (number->string num)))))


(define make-end-label-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "endLabel" (number->string num)))))


(define make-bad-arg-count-label-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "bad_arg_count" (number->string num)))))

(define make-one-arg-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "oneArgForVector" (number->string num)))))

(define make-two-arg-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "twoArgForVector" (number->string num)))))

(define make-bad-input-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "badInputForVector" (number->string num)))))

(define make-two-arg-loop-input-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "twoArgLoopForVector" (number->string num)))))

(define make-two-arg-loop-input-end-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "twoArgLoopEndForVector" (number->string num)))))

(define make-one-arg-loop-input-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "oneArgLoopForVector" (number->string num)))))

(define make-one-arg-loop-input-end-for-make-vector
	(let ((num 200))
			(lambda ()
				(set! num (+ num 1))
				(string-append "oneArgLoopEndForVector" (number->string num)))))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR MAKE-VECTOR EXPRESSION =====================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR MAKE-STRING EXPRESSION ============================================
;=========================================================================================================================================

(define handle_make_string
		(lambda ()
			(let* ((body-label (make-body-label-for-make-string))
				  (no-args-label (make-no-args-label-for-make-string))
				  (end-label (make-end-label-for-make-string))
				  (one-arg-label (make-one-arg-for-make-string))
				  (two-arg-label (make-two-arg-for-make-string))				  
				  (bad-input-label (make-bad-input-for-make-string))
				  (one-arg-loop-label (make-one-arg-loop-input-for-make-string))
				  (one-arg-loop-end-label (make-one-arg-loop-input-end-for-make-string))
				  (two-arg-loop-label (make-two-arg-loop-input-for-make-string))
				  (two-arg-loop-end-label (make-two-arg-loop-input-end-for-make-string)))

				  

				  (string-append (applic-prolog "make_string_code" "end_make_string_code")

				  		"\tmake_string_code:\n\n"
						"\tpush rbp\n"
						"\tmov rbp, rsp\n"


						"\tcmp qword [rbp + 3*8], 1\n"
						"\tje " one-arg-label "\n"

						"\tcmp qword [rbp + 3*8], 2\n"
						"\tje " two-arg-label "\n"
						"\tjmp " bad-input-label "\n"

						one-arg-label ":\n"

						"\tmov rcx, qword [rbp + 4*8]\n"
						"\tmov r10, [rcx]\n"
						"\tTYPE r10\n"
						"\tcmp r10, T_INTEGER\n"
						"\tjne "bad-input-label "\n"

						; Put code for #(0 0 0 0 0) here
						; Initializing the runtime constant zero and putting its address into r1
						"\tmov r12, 0\n"

						; Initializing the first qword of the vector
						"\tmov r8, qword [rbp + 4*8] ; r8 holds a pointer to the string length \n"
						"\tmov r13, [r8]\n"
						"\tDATA r13\n"
						"\tinc r13\n"
						"\tshl r13, 3\n"
						"\tmov rdi, r13\n"
						"\tcall malloc\n"
						"\tmov r13, [r8]\n"
						"\tDATA r13 \n"
						"\tmov qword [rax], r13\n"
						"\tshl qword [rax], 30\n"
						"\tlea r13, [rax + 1*8]\n"
						"\tsub r13, start_of_data\n"
						"\tor qword [rax], r13\n"
						"\tshl qword [rax], TYPE_BITS\n"
						"\tor qword [rax], T_STRING\n"


						; Loop over the string length and initialize its elements to zero char.
						"\tmov r15, [r8]\n"
						"\tDATA r15\n"
						"\tmov r14, 0\n"


						one-arg-loop-label ":\n\n"
						"\tcmp r14, r15\n"
						"\tje " one-arg-loop-end-label "\n"

						"\tmov qword [rax + 1*8 + r14*1], r12\n"
						"\tinc r14\n"
						"\tjmp " one-arg-loop-label "\n"

						one-arg-loop-end-label ":\n\n"
						"\tjmp " end-label "\n"


						two-arg-label":\n\n"

						"\tmov r8, qword [rbp + 4*8]\n"
						"\tmov r10, [r8]\n"
						"\tTYPE r10\n"
						"\tcmp r10, T_INTEGER\n"
						"\tjne "bad-input-label "\n"
						"\tmov r11, [r8]\n"
						"\tDATA r11\n"
						"\tcmp r11, 0\n"
						"\tjl " bad-input-label "\n"


						; Put code for #(4 4 4 4 4) here
						; Setting r12 to the second argument
						"\tmov r8, qword [rbp + 4*8] ; length of the string\n"
						"\tmov r12, qword [rbp + 5*8] ; elements of the string\n"
						"\tmov r12, [r12]\n"
						"\tDATA r12\n"


						; Initializing the first qword of the vector
						"\tmov r11, [r8]\n"
						"\tDATA r11\n"
						"\tinc r11\n"
						"\tshl r11, 3\n"
						"\tmov rdi, r11\n"
						"\tcall malloc\n"
						"\tmov r11, [r8]\n"
						"\tDATA r11\n"
						"\tmov qword [rax], r11\n"
						"\tshl qword [rax], 30\n"
						"\tlea r11, [rax + 1*8]\n"
						"\tsub r11, start_of_data\n"
						"\tor qword [rax], r11\n"
						"\tshl qword [rax], TYPE_BITS\n"
						"\tor qword [rax], T_STRING\n"


						; Loop over the string length and put in its elements the address of the second argument.
						"\tmov r15, [r8]\n"
						"\tDATA r15\n"
						"\tmov r14, 0\n"


						two-arg-loop-label":\n\n"
						"\tcmp r14, r15\n"
						"\tje " two-arg-loop-end-label "\n"

						"\tmov qword [rax + 1*8 + r14*1], r12\n"
						"\tinc r14\n"
						"\tjmp " two-arg-loop-label "\n"

						two-arg-loop-end-label ":\n\n"
						"\tjmp " end-label "\n"


						bad-input-label":\n\n"
						"\tmov rax, sobVoid\n"
						
						end-label ":\n\n"
						"\tmov rsp, rbp\n"
						"\tpop rbp\n"
						"\tret\n"


						"end_make_string_code:\n"
						"\tmov rax, [rax]\n"
						"\tmov qword [makeString], rax\n\n"))))


(define make-body-label-for-make-string
	(let ((num 600))
			(lambda ()
				(set! num (+ num 1))
				(string-append "bodyOfMakeString" (number->string num)))))

(define make-no-args-label-for-make-string
	(let ((num 600))
			(lambda ()
				(set! num (+ num 1))
				(string-append "no_args" (number->string num)))))


(define make-end-label-for-make-string
	(let ((num 600))
			(lambda ()
				(set! num (+ num 1))
				(string-append "endLabel" (number->string num)))))


(define make-one-arg-for-make-string
	(let ((num 600))
			(lambda ()
				(set! num (+ num 1))
				(string-append "oneArgForString" (number->string num)))))

(define make-two-arg-for-make-string
	(let ((num 600))
			(lambda ()
				(set! num (+ num 1))
				(string-append "twoArgForString" (number->string num)))))

(define make-bad-input-for-make-string
	(let ((num 600))
			(lambda ()
				(set! num (+ num 1))
				(string-append "badInputForString" (number->string num)))))

(define make-two-arg-loop-input-for-make-string
	(let ((num 600))
			(lambda ()
				(set! num (+ num 1))
				(string-append "twoArgLoopForString" (number->string num)))))

(define make-two-arg-loop-input-end-for-make-string
	(let ((num 600))
			(lambda ()
				(set! num (+ num 1))
				(string-append "twoArgLoopEndForString" (number->string num)))))

(define make-one-arg-loop-input-for-make-string
	(let ((num 600))
			(lambda ()
				(set! num (+ num 1))
				(string-append "oneArgLoopForString" (number->string num)))))

(define make-one-arg-loop-input-end-for-make-string
	(let ((num 600))
			(lambda ()
				(set! num (+ num 1))
				(string-append "oneArgLoopEndForString" (number->string num)))))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR MAKE-STRING EXPRESSION =====================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR SET-CAR EXPRESSION ================================================
;=========================================================================================================================================


(define handle_set_car
		(lambda ()
			(string-append (applic-prolog "set_car_code" "end_set_car_code")

				"\nset_car_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 2\n"
				"\tjne .badArgs\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_PAIR\n"
				"\tjne .badArgs\n"

				"\tmov r13, qword [rbp + 8*4] ; r13 holds a pointer to the pair\n"
				"\tmov r13, [r13] ; hold the actual pair\n"
				"\tmov r12, qword [rbp + 8*5] ; r12 holds a pointer to the element to replace the car element of the pair \n"
				"\tsub r12, start_of_data\n"

				"\tshl r13, 30\n"
				"\tshr r13, 30\n"
				"\tshl r12, 34\n" 
				"\tor r13,r12\n" 
				"\tmov [rax], r13\n"

				"\tmov rax, sobVoid\n"
				"\tjmp .done\n\n"
				".badArgs:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_set_car_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [setCar], rax\n\n")))




;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR SET-CAR EXPRESSION =========================================
;=========================================================================================================================================

;=========================================================================================================================================
;======================================================= FUNCTIONS FOR SET-CDR EXPRESSION ================================================
;=========================================================================================================================================


(define handle_set_cdr
		(lambda ()
			(string-append (applic-prolog "set_cdr_code" "end_set_cdr_code")

				"\nset_cdr_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 2\n"
				"\tjne .badArgs\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_PAIR\n"
				"\tjne .badArgs\n"

				"\tmov r13, qword [rbp + 8*4] ; r13 holds a pointer to the pair\n"
				"\tmov r13, [r13] ; hold the actual pair\n"
				"\tmov r12, qword [rbp + 8*5] ; r12 holds a pointer to the element to replace the cdr element of the pair \n"
				"\tsub r12, start_of_data\n"

				"\tshr r13, 34\n"
				"\tshl r13, 34\n"
				"\tor r13, T_PAIR\n"
				"\tshl r12, 4\n" 
				"\tor r13,r12\n" 
				"\tmov [rax], r13\n"

				"\tmov rax, sobVoid\n"
				"\tjmp .done\n\n"
				".badArgs:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_set_cdr_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [setCdr], rax\n\n")))




;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR SET-CDR EXPRESSION =========================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR STRING-SET EXPRESSION =============================================
;=========================================================================================================================================

(define handle_string_set
		(lambda () 
			(string-append (applic-prolog "string_set_code" "end_string_set_code")

				"\nstring_set_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 3\n"
				"\tjl .badArgs\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_STRING\n"
				"\tjne .badArgs\n"
				"\tmov rcx, qword [rbp + 8*5]\n"
				"\tmov r10, [rcx]\n"
				"\tTYPE r10\n"
				"\tcmp r10, T_INTEGER\n"
				"\tjne .badArgs\n"
				"\tmov r15, qword [rbp + 8*6]\n"
				"\tmov r15, [r15]\n"
				"\tTYPE r15\n"
				"\tcmp r15, T_CHAR\n"
				"\tjne .badArgs\n"
				"\tmov r13, qword [rbp + 8*4] ; rbx holds a pointer to the string\n"
				"\tmov r13, [r13] ; hold the actual string\n"
				"\tmov r9, qword [rbp + 8*5]\n"
				"\tmov r10, [r9] ; r10 hold k - the position in the string\n"
				"\tDATA r10\n"
				"\tmov rbx, qword [rbp + 8*6] ; r12 holds a pointer to the char to replace the k-th element of the string \n"
				"\tmov rbx, [rbx]\n"
				"\tDATA rbx\n"
				"\tSTRING_ELEMENTS r13\n"
				"\tmov byte [r13 + r10*1], bl\n"
				"\tmov rax, sobVoid\n"
				"\tjmp .done\n\n"
				".badArgs:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_string_set_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [stringSet], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR STRING-SET EXPRESSION ======================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR STRING-REF EXPRESSION =============================================
;=========================================================================================================================================

(define handle_string_ref
		(lambda () 

			(string-append (applic-prolog "string_ref_code" "end_string_ref_code")

				"\nstring_ref_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 2\n"
				"\tjne .badArgs\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_STRING\n"
				"\tjne .badArgs\n"
				"\tmov rcx, qword [rbp + 8*5]\n"
				"\tmov r10, [rcx]\n"
				"\tTYPE r10\n"
				"\tcmp r10, T_INTEGER\n"
				"\tjne .badArgs\n"
				"\tmov rbx, qword [rbp + 8*4]\n"
				"\tmov r11, [rbx] ; the string \n"
				"\tmov r9, qword [rbp + 8*5]\n"
				"\tmov r10, [r9] ; the position in the string\n"
				"\tDATA r10\n"
				"\tSTRING_ELEMENTS r11 ; the individual chars of the string\n"
				"\tadd r11, r10\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov r11, qword [r11]\n"
				"\tshl r11, 4\n"
				"\tor r11, T_CHAR\n"
				"\tmov qword [rax], r11\n"
				"\tjmp .done\n\n"
				".badArgs:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_string_ref_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [stringRef], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR STRING-REF EXPRESSION ======================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR STRING-LENGTH EXPRESSION ==========================================
;=========================================================================================================================================

(define handle_string_length
		(lambda () 

			(string-append (applic-prolog "string_length_code" "end_string_length_code")

				"\nstring_length_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notAString\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_STRING\n"
				"\tjne .notAString\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tSTRING_LENGTH r10\n"
				"\tshl r10, 4\n"
				"\tor r10, T_INTEGER\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r10\n"
				"\tjmp .done\n\n"
				".notAString:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_string_length_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [stringLength], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR STRING-LENGTH EXPRESSION ===================================
;=========================================================================================================================================

;=========================================================================================================================================
;======================================================= FUNCTIONS FOR EQUAL EXPRESSION ==================================================
;=========================================================================================================================================

(define handle_equal
		(lambda () 

			(string-append (applic-prolog "equal_code" "end_equal_code")

				"equal_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .check_equal\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
				"\tmov rbx, [rbx]\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .incCounter\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .incCounter\n"
				"\tjmp .badArgs\n"
				".incCounter:\n\n"
				"\tinc rcx\n"
				"\tjmp .checkIfArgsAreNumbers\n"


				".check_number_of_args:\n\n"
				"\tmov rcx, qword [rbp + 8*3]\n"
				"\tcmp rcx, 1\n"
				"\tje .retTrue\n"

				".check_equal:\n\n"
				"\tmov r8, 0 ; r8 is a counter for the number of arguments\n"
				"\tmov r9, qword [rbp + 8*3]\n"
				"\tdec r9\n"


				".check_equal_loop:\n\n"
				"\tcmp r8,r9\n"
				"\tje .retTrue\n\n"
				"\tmov r10, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r10]\n"
				"\tmov r11, r10\n"
				"\tTYPE r10\n"
				"\tcmp r10, T_FRACTION\n"
				"\tjne .makeFirstFraction\n"
				"\tmov r10, r11\n"
				"\tNUMERATOR r10 ; holds the numerator -a- of the first number\n"
				"\tDATA r10\n"
				"\tDENOMINATOR r11 ; holds the denominator -b- of the second number\n"
				"\tDATA r11\n"
				".continueComparingAfterFirst:\n\n"
				"\tmov r12, qword [rbp + 8*(5 + r8)]\n"
				"\tmov r12, [r12]\n"
				"\tmov r13, r12\n"
				"\tTYPE r12\n"
				"\tcmp r12, T_FRACTION\n"
				"\tjne .makeSecondFraction\n"
				"\tmov r12, r13\n"
				"\tNUMERATOR r12 ; holds the numerator -c- of the number to be added\n"
				"\tDATA r12\n"
				"\tDENOMINATOR r13 ; holds the denominator -d- of the number to be added\n"
				"\tDATA r13\n"
				".continueComparingAfterSecond:\n\n"
				"\tmov rax, r10\n"
				"\tmul r13\n"
				"\tmov r10, rax\n"
				"\tmov rax, r11\n"
				"\tmul r12\n"
				"\tmov r12, rax\n"
				"\tcmp r10, r12\n"
				"\tjne .retFalse\n"
				"\tinc r8\n"
				"\tjmp .check_equal_loop\n"

				".makeFirstFraction:\n\n"
				"\tmov r10, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r10]\n"
				"\tDATA r10\n"
				"\tmov r11, 1\n"
				"\tjmp .continueComparingAfterFirst\n"

				".makeSecondFraction:\n\n"
				"\tmov r12, qword [rbp + 8*(5 + r8)]\n"
				"\tmov r12, [r12]\n"
				"\tDATA r12\n"
				"\tmov r13, 1\n"
				"\tjmp .continueComparingAfterSecond\n"

				".retFalse:\n\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n"

				".retTrue:\n\n"
				"\tmov rax, sobTrue\n"
				"\tjmp .done\n"

				".badArgs:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_equal_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [equal], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR EQUAL EXPRESSION ===========================================
;=========================================================================================================================================




;=========================================================================================================================================
;======================================================= FUNCTIONS FOR GREATER-THAN EXPRESSION ===========================================
;=========================================================================================================================================

(define handle_greater_than
		(lambda () 

			(string-append (applic-prolog "greater_than_code" "end_greater_than_code")

				"greater_than_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .check_greater_than\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
				"\tmov rbx, [rbx]\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .incCounter\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .incCounter\n"
				"\tjmp .badArgs\n"
				".incCounter:\n\n"
				"\tinc rcx\n"
				"\tjmp .checkIfArgsAreNumbers\n"


				".check_number_of_args:\n\n"
				"\tmov rcx, qword [rbp + 8*3]\n"
				"\tcmp rcx, 1\n"
				"\tje .retTrue\n"

				".check_greater_than:\n\n"
				"\tmov r8, 0 ; r8 is a counter for the number of arguments\n"
				"\tmov r9, qword [rbp + 8*3]\n"
				"\tdec r9\n"


				".check_greater_than_loop:\n\n"
				"\tcmp r8,r9\n"
				"\tje .retTrue\n\n"
				"\tmov r10, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r10]\n"
				"\tmov r11, r10\n"
				"\tTYPE r10\n"
				"\tcmp r10, T_FRACTION\n"
				"\tjne .makeFirstFraction\n"
				"\tmov r10, r11\n"
				"\tNUMERATOR r10 ; holds the numerator -a- of the first number\n"
				"\tDATA r10\n"
				"\tDENOMINATOR r11 ; holds the denominator -b- of the second number\n"
				"\tDATA r11\n"
				".continueComparingAfterFirst:\n\n"
				"\tmov r12, qword [rbp + 8*(5 + r8)]\n"
				"\tmov r12, [r12]\n"
				"\tmov r13, r12\n"
				"\tTYPE r12\n"
				"\tcmp r12, T_FRACTION\n"
				"\tjne .makeSecondFraction\n"
				"\tmov r12, r13\n"
				"\tNUMERATOR r12 ; holds the numerator -c- of the number to be added\n"
				"\tDATA r12\n"
				"\tDENOMINATOR r13 ; holds the denominator -d- of the number to be added\n"
				"\tDATA r13\n"
				".continueComparingAfterSecond:\n\n"
				"\tmov rax, r10\n"
				"\tmul r13\n"
				"\tmov r10, rax\n"
				"\tmov rax, r11\n"
				"\tmul r12\n"
				"\tmov r12, rax\n"
				"\tcmp r10, r12\n"
				"\tjle .retFalse\n"
				"\tinc r8\n"
				"\tjmp .check_greater_than_loop\n"

				".makeFirstFraction:\n\n"
				"\tmov r10, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r10]\n"
				"\tDATA r10\n"
				"\tmov r11, 1\n"
				"\tjmp .continueComparingAfterFirst\n"

				".makeSecondFraction:\n\n"
				"\tmov r12, qword [rbp + 8*(5 + r8)]\n"
				"\tmov r12, [r12]\n"
				"\tDATA r12\n"
				"\tmov r13, 1\n"
				"\tjmp .continueComparingAfterSecond\n"

				".retFalse:\n\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n"

				".retTrue:\n\n"
				"\tmov rax, sobTrue\n"
				"\tjmp .done\n"

				".badArgs:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_greater_than_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [greaterThan], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR GREATER-THAN EXPRESSION ====================================
;=========================================================================================================================================

;=========================================================================================================================================
;======================================================= FUNCTIONS FOR LESS-THAN EXPRESSION ==============================================
;=========================================================================================================================================

(define handle_less_than
		(lambda () 

			(string-append (applic-prolog "less_than_code" "end_less_than_code")

				"less_than_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .check_less_than\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
				"\tmov rbx, [rbx]\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .incCounter\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .incCounter\n"
				"\tjmp .badArgs\n"
				".incCounter:\n\n"
				"\tinc rcx\n"
				"\tjmp .checkIfArgsAreNumbers\n"


				".check_number_of_args:\n\n"
				"\tmov rcx, qword [rbp + 8*3]\n"
				"\tcmp rcx, 1\n"
				"\tje .retTrue\n"

				".check_less_than:\n\n"
				"\tmov r8, 0 ; r8 is a counter for the number of arguments\n"
				"\tmov r9, qword [rbp + 8*3]\n"
				"\tdec r9\n"


				".check_less_than_loop:\n\n"
				"\tcmp r8,r9\n"
				"\tje .retTrue\n\n"
				"\tmov r10, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r10]\n"
				"\tmov r11, r10\n"
				"\tTYPE r10\n"
				"\tcmp r10, T_FRACTION\n"
				"\tjne .makeFirstFraction\n"
				"\tmov r10, r11\n"
				"\tNUMERATOR r10 ; holds the numerator -a- of the first number\n"
				"\tDATA r10\n"
				"\tDENOMINATOR r11 ; holds the denominator -b- of the second number\n"
				"\tDATA r11\n"
				".continueComparingAfterFirst:\n\n"
				"\tmov r12, qword [rbp + 8*(5 + r8)]\n"
				"\tmov r12, [r12]\n"
				"\tmov r13, r12\n"
				"\tTYPE r12\n"
				"\tcmp r12, T_FRACTION\n"
				"\tjne .makeSecondFraction\n"
				"\tmov r12, r13\n"
				"\tNUMERATOR r12 ; holds the numerator -c- of the number to be added\n"
				"\tDATA r12\n"
				"\tDENOMINATOR r13 ; holds the denominator -d- of the number to be added\n"
				"\tDATA r13\n"
				".continueComparingAfterSecond:\n\n"
				"\tmov rax, r10\n"
				"\tmul r13\n"
				"\tmov r10, rax\n"
				"\tmov rax, r11\n"
				"\tmul r12\n"
				"\tmov r12, rax\n"
				"\tcmp r12, r10\n"
				"\tjle .retFalse\n"
				"\tinc r8\n"
				"\tjmp .check_less_than_loop\n"

				".makeFirstFraction:\n\n"
				"\tmov r10, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r10]\n"
				"\tDATA r10\n"
				"\tmov r11, 1\n"
				"\tjmp .continueComparingAfterFirst\n"

				".makeSecondFraction:\n\n"
				"\tmov r12, qword [rbp + 8*(5 + r8)]\n"
				"\tmov r12, [r12]\n"
				"\tDATA r12\n"
				"\tmov r13, 1\n"
				"\tjmp .continueComparingAfterSecond\n"

				".retFalse:\n\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n"

				".retTrue:\n\n"
				"\tmov rax, sobTrue\n"
				"\tjmp .done\n"

				".badArgs:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_less_than_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [lessThan], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR LESS-THAN EXPRESSION =======================================
;=========================================================================================================================================


;set disassembly-flavor intel
;layout split
;layout regs


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR MULTIPLY EXPRESSION ===============================================
;=========================================================================================================================================

(define handle_multiply
		(lambda () 

			(string-append (applic-prolog "multiply_code" "end_multiply_code")

				"multiply_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .make_multiplication\n"
				"\tmov rax, qword [rbp + 8*(4 + rcx)]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .incCounter\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .incCounter\n"
				"\tjmp .badArgs\n"
				".incCounter:\n\n"
				"\tinc rcx\n"
				"\tjmp .checkIfArgsAreNumbers\n"


				".make_multiplication:\n\n"
				"\tmov r12, 1 ; represent the numerator -a- of the sum\n"
				"\tmov r13, 1 ; represent the denominator -b- of the sum\n"

				"\tmov r8, 0\n"

				".multiplication_loop:\n\n"
				"\tcmp r8, qword [rbp + 8*3]\n"
				"\tje .doneMultiplication\n\n"
				"\tmov r9, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r9]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tjne .makeFraction\n"
				"\tmov r11, r10\n"
				"\tNUMERATOR r10 ; holds the numerator -c- of the number to be added\n"
				"\tDATA r10\n"
				"\tDENOMINATOR r11 ; holds the denominator -d- of the number to be added\n"
				"\tDATA r11\n"
				".continueMultiplying:\n"
				"\tmov rax, r10\n"
				"\tmul r12\n"
				"\tmov r12, rax ; r12 gets the new numerator\n"
				"\tmov rax, r11\n"
				"\tmul r13\n"
				"\tmov r13, rax ; r13 gets the new denominator\n"
				"\tinc r8\n"
				"\tjmp .multiplication_loop\n"

				".makeFraction:\n\n"
				"\tmov r9, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r9]\n"
				"\tDATA r10\n"
				"\tmov r11, 1\n"
				"\tjmp .continueMultiplying\n"

				".doneMultiplication:\n\n"
				"\tpush r12\n"
				"\tpush r13\n"
				"\tmov rax, 0\n"
				"\tcall gcd\n"
				"\tmov r10, rax\n"
				"\tmov rax, r12\n"
				"\tcqo\n"
				"\tidiv r10\n"
				"\tmov r12, rax\n"
				"\tmov rax, r13\n"
				"\tcqo\n"
				"\tidiv r10\n"
				"\tmov r13, rax\n"
				"\tcmp r13, 1\n"
				"\tje .retInt\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov r14, rax\n"
				"\tshl r12, TYPE_BITS\n"
				"\tor r12, T_INTEGER\n"
				"\tmov qword [r14], r12 ; r14 hold the numerator of the result\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov r15, rax\n"
				"\tshl r13, TYPE_BITS\n"
				"\tor r13, T_INTEGER\n"
				"\tmov qword [r15], r13 ; r15 hold the denominator of the result\n"

				"\tmov r8, r14\n"
				"\tsub r8, start_of_data\n"
				"\tshl r8, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)\n"
				"\tmov r9, r15\n"
				"\tsub r9, start_of_data\n"
				"\tshl r9, TYPE_BITS\n"
				"\tor r8, r9\n"
				"\tor r8, T_FRACTION\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r8\n"
				"\tjmp .done\n"

				".retInt:\n\n"
				"\tshl r12, TYPE_BITS\n"
				"\tor r12, T_INTEGER\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r12\n"
				"\tjmp .done\n"


				".badArgs:\n\n"
				"\tmov rax, sobVoid\n"

				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_multiply_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [multiply], rax\n\n")))



;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR MULTIPLY EXPRESSION ========================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR DIVIDE EXPRESSION =================================================
;=========================================================================================================================================

(define handle_divide
		(lambda () 

			(string-append (applic-prolog "divide_code" "end_divide_code")

				"divide_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .make_division\n"
				"\tmov rax, qword [rbp + 8*(4 + rcx)]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .incCounter\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .incCounter\n"
				"\tjmp .badArgs\n"
				".incCounter:\n\n"
				"\tinc rcx\n"
				"\tjmp .checkIfArgsAreNumbers\n"


				".make_division:\n\n"
				;"\tmov r12, 1 ; represent the numerator -a- of the sum\n"
				;"\tmov r13, 1 ; represent the denominator -b- of the sum\n"

				"\tmov r12, qword [rbp + 4*8]\n"
				"\tmov r12, [r12]\n"
				"\tTYPE r12\n"
				"\tcmp r12, T_FRACTION\n"
				"\tje .startWithFraction\n"
				"\tmov r12, qword [rbp + 4*8]\n"
				"\tmov r12, [r12]\n"
				"\tDATA r12 ; represent the numerator -a- of the sum\n"
				"\tmov r13, 1 ; represent the denominator -b- of the sum\n"
				

				".startDivision:\n\n"


				"\tmov r8, 1\n"

				".division_loop:\n\n"
				"\tcmp r8, qword [rbp + 8*3]\n"
				"\tje .doneDivision\n\n"
				"\tmov r9, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r9]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tjne .makeFraction\n"
				"\tmov r11, r10\n"
				"\tNUMERATOR r10 ; holds the numerator -c- of the number to be added\n"
				"\tDATA r10\n"
				"\tDENOMINATOR r11 ; holds the denominator -d- of the number to be added\n"
				"\tDATA r11\n"
				"\txchg r10, r11\n"
				".continueDividing:\n"
				"\tmov rax, r10\n"
				"\tmul r12\n"
				"\tmov r12, rax ; r12 gets the new numerator\n"
				"\tmov rax, r11\n"
				"\tmul r13\n"
				"\tmov r13, rax ; r13 gets the new denominator\n"
				"\tinc r8\n"
				"\tjmp .division_loop\n"

				".startWithFraction:\n\n"
				"\tmov r12, qword [rbp + 4*8]\n"
				"\tmov r12, [r12]\n"
				"\tmov r13, r12\n"
				"\tNUMERATOR r12 ; holds the numerator -c- of the number to be added\n"
				"\tDATA r12\n"
				"\tDENOMINATOR r13 ; holds the denominator -d- of the number to be added\n"
				"\tDATA r13\n"
				"\tjmp .startDivision\n"

				".makeFraction:\n\n"
				"\tmov r9, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r11, [r9]\n"
				"\tDATA r11\n"
				"\tmov r10, 1\n"
				"\tjmp .continueDividing\n"

				".doneDivision:\n\n"
				"\tcmp r13, 0\n"
				"\tjl .negFrac\n"
				".createFrac:\n"
				"\tpush r12\n"
				"\tpush r13\n"
				"\tmov rax, 0\n"
				"\tcall gcd\n"
				"\tmov r10, rax\n"
				"\tmov rax, r12\n"
				"\tcqo\n"
				"\tidiv r10\n"
				"\tmov r12, rax\n"
				"\tmov rax, r13\n"
				"\tcqo\n"
				"\tidiv r10\n"
				"\tmov r13, rax\n"
				"\tcmp r13, 1\n"
				"\tje .retInt\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov r14, rax\n"
				"\tshl r12, TYPE_BITS\n"
				"\tor r12, T_INTEGER\n"
				"\tmov qword [r14], r12 ; r14 hold the numerator of the result\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov r15, rax\n"
				"\tshl r13, TYPE_BITS\n"
				"\tor r13, T_INTEGER\n"
				"\tmov qword [r15], r13 ; r15 hold the denominator of the result\n"

				"\tmov r8, r14\n"
				"\tsub r8, start_of_data\n"
				"\tshl r8, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)\n"
				"\tmov r9, r15\n"
				"\tsub r9, start_of_data\n"
				"\tshl r9, TYPE_BITS\n"
				"\tor r8, r9\n"
				"\tor r8, T_FRACTION\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r8\n"
				"\tjmp .done\n"

				".retInt:\n\n"
				"\tshl r12, TYPE_BITS\n"
				"\tor r12, T_INTEGER\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r12\n"
				"\tjmp .done\n"

				".negFrac:\n\n"
				"\tneg r12\n"
				"\tneg r13\n"
				"\tjmp .createFrac\n"

				".badArgs:\n\n"
				"\tmov rax, sobVoid\n"

				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_divide_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [divide], rax\n\n")))



;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR DIVIDE EXPRESSION ==========================================
;=========================================================================================================================================

;=========================================================================================================================================
;======================================================= FUNCTIONS FOR PLUS EXPRESSION ===================================================
;=========================================================================================================================================

(define handle_plus
		(lambda () 

			(string-append (applic-prolog "plus_code" "end_plus_code")

				"plus_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .make_addition\n"
				"\tmov rax, qword [rbp + 8*(4 + rcx)]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .incCounter\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .incCounter\n"
				"\tjmp .badArgs\n"
				".incCounter:\n\n"
				"\tinc rcx\n"
				"\tjmp .checkIfArgsAreNumbers\n"


				".make_addition:\n\n"
				"\tmov r12, 0 ; represent the numerator -a- of the sum\n"
				"\tmov r13, 1 ; represent the denominator -b- of the sum\n"

				"\tmov r8, 0\n"

				".addition_loop:\n\n"
				"\tcmp r8, qword [rbp + 8*3]\n"
				"\tje .doneAddition\n\n"
				"\tmov r9, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r9]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tjne .makeFraction\n"
				"\tmov r11, r10\n"
				"\tNUMERATOR r10 ; holds the numerator -c- of the number to be added\n"
				"\tDATA r10\n"
				"\tDENOMINATOR r11 ; holds the denominator -d- of the number to be added\n"
				"\tDATA r11\n"
				".continueSumming:\n"
				"\tmov rax, r10\n"
				"\tmul r13\n"
				"\tmov rcx, rax ; rcx temporeraly hold a*d\n"
				"\tmov rax, r11\n"
				"\tmul r12\n"
				"\tadd rcx, rax ; rcx temporeraly hold a*d + b*c\n"
				"\tmov r12, rcx ; r12 gets the new numerator\n"
				"\tmov rax, r11\n"
				"\tmul r13; rax temporeraly hold a*d\n"
				"\tmov r13, rax ; r12 gets the new denominator\n"
				"\tinc r8\n"
				"\tjmp .addition_loop\n"

				".makeFraction:\n\n"
				"\tmov r9, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r9]\n"
				"\tDATA r10\n"
				"\tmov r11, 1\n"
				"\tjmp .continueSumming\n"

				".doneAddition:\n\n"
				"\tpush r12\n"
				"\tpush r13\n"
				"\tmov rax, 0\n"
				"\tcall gcd\n"
				"\tmov r10, rax\n"
				"\tmov rax, r12\n"
				"\tcqo\n"
				"\tidiv r10\n"
				"\tmov r12, rax\n"
				"\tmov rax, r13\n"
				"\tcqo\n"
				"\tidiv r10\n"
				"\tmov r13, rax\n"
				"\tcmp r13, 1\n"
				"\tje .retInt\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov r14, rax\n"
				"\tshl r12, TYPE_BITS\n"
				"\tor r12, T_INTEGER\n"
				"\tmov qword [r14], r12 ; r14 hold the numerator of the result\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov r15, rax\n"
				"\tshl r13, TYPE_BITS\n"
				"\tor r13, T_INTEGER\n"
				"\tmov qword [r15], r13 ; r15 hold the denominator of the result\n"

				"\tmov r8, r14\n"
				"\tsub r8, start_of_data\n"
				"\tshl r8, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)\n"
				"\tmov r9, r15\n"
				"\tsub r9, start_of_data\n"
				"\tshl r9, TYPE_BITS\n"
				"\tor r8, r9\n"
				"\tor r8, T_FRACTION\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r8\n"
				"\tjmp .done\n"

				".retInt:\n\n"
				"\tshl r12, TYPE_BITS\n"
				"\tor r12, T_INTEGER\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r12\n"
				"\tjmp .done\n"


				".badArgs:\n\n"
				"\tmov rax, sobVoid\n"

				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_plus_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [plus], rax\n\n")))



;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR PLUS EXPRESSION ============================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR SUBTRACT EXPRESSION ==============================================
;=========================================================================================================================================

(define handle_subtract
		(lambda () 

			(string-append (applic-prolog "subtract_code" "end_subtract_code")

				"subtract_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .make_subtraction\n"
				"\tmov rax, qword [rbp + 8*(4 + rcx)]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .incCounter\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .incCounter\n"
				"\tjmp .badArgs\n"
				".incCounter:\n\n"
				"\tinc rcx\n"
				"\tjmp .checkIfArgsAreNumbers\n"


				".make_subtraction:\n\n"
				;"\tmov r12, 0 ; represent the numerator -a- of the sum\n"
				;"\tmov r13, 1 ; represent the denominator -b- of the sum\n"

				"\tmov r12, qword [rbp + 4*8]\n"
				"\tmov r12, [r12]\n"
				"\tTYPE r12\n"
				"\tcmp r12, T_FRACTION\n"
				"\tje .startWithFraction\n"
				"\tmov r12, qword [rbp + 4*8]\n"
				"\tmov r12, [r12]\n"
				"\tDATA r12 ; represent the numerator -a- of the sum\n"
				"\tmov r13, 1 ; represent the denominator -b- of the sum\n"
				".startSubstraction:\n\n"

				"\tmov r8, 1\n"

				".subtraction_loop:\n\n"
				"\tcmp r8, qword [rbp + 8*3]\n"
				"\tje .doneSubtraction\n\n"
				"\tmov r9, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r9]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tjne .makeFraction\n"
				"\tmov r11, r10\n"
				"\tNUMERATOR r10 ; holds the numerator -c- of the number to be added\n"
				"\tDATA r10\n"
				"\tDENOMINATOR r11 ; holds the denominator -d- of the number to be added\n"
				"\tDATA r11\n"
				".continueSubtracting:\n"
				"\tmov rax, r10\n"
				"\tmul r13\n"
				"\tmov rcx, rax ; rcx temporeraly hold a*d\n"
				"\tmov rax, r11\n"
				"\tmul r12\n"
				"\tsub rcx, rax ; rcx temporeraly hold a*d - b*c\n"
				"\tmov r12, rcx ; r12 gets the new numerator\n"
				"\tneg r12\n"
				"\tmov rax, r11\n"
				"\tmul r13; rax temporeraly hold a*d\n"
				"\tmov r13, rax ; r12 gets the new denominator\n"
				"\tinc r8\n"
				"\tjmp .subtraction_loop\n"

				".makeFraction:\n\n"
				"\tmov r9, qword [rbp + 8*(4 + r8)]\n"
				"\tmov r10, [r9]\n"
				"\tDATA r10\n"
				"\tmov r11, 1\n"
				"\tjmp .continueSubtracting\n"

				".startWithFraction:\n\n"
				"\tmov r12, qword [rbp + 4*8]\n"
				"\tmov r12, [r12]\n"
				"\tmov r13, r12\n"
				"\tNUMERATOR r12 ; holds the numerator -c- of the number to be added\n"
				"\tDATA r12\n"
				"\tDENOMINATOR r13 ; holds the denominator -d- of the number to be added\n"
				"\tDATA r13\n"
				"\tjmp .startSubstraction\n"

				".doneSubtraction:\n\n"
				"\tpush r12\n"
				"\tpush r13\n"
				"\tmov rax, 0\n"
				"\tcall gcd\n"
				"\tmov r10, rax\n"
				"\tmov rax, r12\n"
				"\tcqo\n"
				"\tidiv r10\n"
				"\tmov r12, rax\n"
				"\tmov rax, r13\n"
				"\tcqo\n"
				"\tidiv r10\n"
				"\tmov r13, rax\n"
				"\tcmp r13, 1\n"
				"\tje .retInt\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov r14, rax\n"
				"\tshl r12, TYPE_BITS\n"
				"\tor r12, T_INTEGER\n"
				"\tmov qword [r14], r12 ; r14 hold the numerator of the result\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov r15, rax\n"
				"\tshl r13, TYPE_BITS\n"
				"\tor r13, T_INTEGER\n"
				"\tmov qword [r15], r13 ; r15 hold the denominator of the result\n"

				"\tmov r8, r14\n"
				"\tsub r8, start_of_data\n"
				"\tshl r8, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)\n"
				"\tmov r9, r15\n"
				"\tsub r9, start_of_data\n"
				"\tshl r9, TYPE_BITS\n"
				"\tor r8, r9\n"
				"\tor r8, T_FRACTION\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r8\n"
				"\tjmp .done\n"

				".retInt:\n\n"
				"\tshl r12, TYPE_BITS\n"
				"\tor r12, T_INTEGER\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r12\n"
				"\tjmp .done\n"


				".badArgs:\n\n"
				"\tmov rax, sobVoid\n"

				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_subtract_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [subtract], rax\n\n")))



;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR SUBTRACT EXPRESSION =======================================
;=========================================================================================================================================




;=========================================================================================================================================
;======================================================= FUNCTIONS FOR CHAR->INTEGER EXPRESSION ==========================================
;=========================================================================================================================================

(define handle_char->integer
		(lambda () 

			(string-append (applic-prolog "char_to_integer_code" "end_char_to_integer_code")

				"char_to_integer_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_CHAR\n"
				"\tjne .badInput ; not of type char - can't convert\n"
				"\txor r10, (T_INTEGER ^ T_CHAR)\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov [rax], r10\n"
				"\tjmp .done\n\n"
				".badInput:\n\n"
				"\tmov rax, sobVoid\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_char_to_integer_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [charToInteger], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR CHAR->INTEGER EXPRESSION ===================================
;=========================================================================================================================================




;=========================================================================================================================================
;======================================================= FUNCTIONS FOR INTEGER->CHAR EXPRESSION ==========================================
;=========================================================================================================================================

(define handle_integer->char
		(lambda () 

			(string-append (applic-prolog "integer_to_char_code" "end_integer_to_char_code")

				"integer_to_char_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tjne .badInput ; not of type integer - can't convert\n"
				"\tmov rbx, r10\n"
				"\tDATA rbx\n"
				"\tcmp rbx, 0\n"
				"\tjl .badInput ; negative integer - can't convert to char because it doesn't have an ascii representation of type integer - can't convert\n"
				"\tmov rbx, r10\n"
				"\tDATA rbx\n"
				"\tcmp rbx, 256\n"
				"\tjge .badInput ; integer to large - can't convert to char because it doesn't have an ascii representation of type integer - can't convert\n"
				"\txor r10, (T_CHAR ^ T_INTEGER)\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov [rax], r10\n"
				"\tjmp .done\n\n"
				".badInput:\n\n"
				"\tmov rax, sobVoid\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_integer_to_char_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [integerToChar], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR INTEGER->CHAR EXPRESSION ===================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR REMAINDER EXPRESSION ==============================================
;=========================================================================================================================================

(define handle_remainder
		(lambda () 

			(string-append (applic-prolog "remainder_code" "end_remainder_code")

				"remainder_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 2\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tjne .badArgs\n"
				"\tmov rdx, qword [rbp + 8*5]\n"
				"\tmov r10, [rdx]\n"
				"\tmov rdx, r10\n"
				"\tTYPE rdx\n"
				"\tcmp rdx, T_INTEGER\n"
				"\tjne .badArgs\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rax, [rax]\n"
				"\tDATA rax ; rax hold the dividend \n"
				"\tmov rcx, qword [rbp + 8*5]\n"
				"\tmov r10, [rcx]\n"
				"\tDATA r10 ; r10 holds the divisor\n"
				"\tcmp rax, 0\n"
				"\tjl .firstArgsIsNeg\n"
				"\tcmp r10, 0\n"
				"\tjl .secondArgIsNeg\n"
				;"\tcmp rax, 0\n"
				"\tmov rdx, 0\n"
				"\tdiv r10\n"
				"\tmov r15, rdx\n"
				;"\tneg r15\n";;;;;;;;;;;;;;;
				"\tshl r15, 4\n"
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				"\tor r15, T_INTEGER\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r15\n"
				"\tjmp .done\n"

				".firstArgsIsNeg:\n\n"
				"\tmov r15, 1\n"
				"\tneg rax\n"
				"\tcmp r10, 0\n"
				"\tjl .secondArgIsNeg\n"
				"\tjmp .devidendIsNeg\n"

				".secondArgIsNeg:\n\n"
				"\tneg r10\n"
				"\tcmp r15, 1\n"
				"\tje .devidendIsNeg\n"
				"\tmov rdx, 0\n"
				"\tdiv r10\n"
				"\tmov r15, rdx\n"
				;"\tneg r15\n";;;;;;;;;;;;;;;;;;
				"\tshl r15, 4\n"
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				"\tor r15, T_INTEGER\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r15\n"
				"\tjmp .done\n"


				".devidendIsNeg:\n\n"
				"\tmov rdx, 0\n"
				"\tdiv r10\n"
				"\tmov r15, rdx\n"
				"\tneg r15\n";;;;;;;;;;;;;;;;;;
				"\tshl r15, 4\n"
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				"\tor r15, T_INTEGER\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov qword [rax], r15\n"
				"\tjmp .done\n"


				".badArgCount:\n"
				"\tmov rax, sobVoid\n"
				"\tjmp .done\n\n"
				".badArgs:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_remainder_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [remainder], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR REMAINDER EXPRESSION =======================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR NUMERATOR EXPRESSION ==============================================
;=========================================================================================================================================

(define handle_numerator
		(lambda () 

			(string-append (applic-prolog "numerator_code" "end_numerator_code")

				"numerator_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notAFraction\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .done\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tjne .notAFraction\n"
				"\tRUNTIME_NUMERATOR r10\n"
				"\tmov rax, r10\n"
				"\tjmp .done\n\n"
				".notAFraction:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_numerator_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [numerator], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR NUMERATOR EXPRESSION =======================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR DENOMINATOR EXPRESSION ============================================
;=========================================================================================================================================

(define handle_denominator
		(lambda () 

			(string-append (applic-prolog "denominator_code" "end_denominator_code")

				"\ndenominator_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notAFraction\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .returnOne\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tjne .notAFraction\n"
				"\tRUNTIME_DENOMINATOR r10\n"
				"\tmov rax, r10\n"
				"\tjmp .done\n\n"
				".notAFraction:\n"
				"\tmov rax, sobVoid\n"
				"\tjmp .done\n\n"
				".returnOne:\n\n"
				"\tmov rdi, 8\n"
				"\tmov qword [rax], MAKE_LITERAL(T_INTEGER, 1)\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"
				
				"end_denominator_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [denominator], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR DENOMINATOR EXPRESSION =====================================
;=========================================================================================================================================






;=========================================================================================================================================
;======================================================= FUNCTIONS FOR CONS EXPRESSION ===================================================
;=========================================================================================================================================

(define handle_cons
		(lambda () 

			(string-append (applic-prolog "cons_code" "end_cons_code")

				"\ncons_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov r13, qword [rbp + 8*3]\n"
				"\tcmp r13, 2\n"
				"\tjne .badArgCount\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov r14, [rbp + 4*8]\n"
				"\tmov r15, [rbp + 5*8]\n"
				"\tsub r14, start_of_data\n"
				"\tshl r14, 30\n"
				"\tsub r15, start_of_data\n"
				"\tor r14, r15\n"
				"\tshl r14, 4\n"
				"\tor r14, T_PAIR\n"
				"\tmov [rax], r14\n"
				"\tjmp .done\n\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_cons_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [cons], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR CONS EXPRESSION ============================================
;=========================================================================================================================================




;=========================================================================================================================================
;======================================================= FUNCTIONS FOR CAR EXPRESSION ====================================================
;=========================================================================================================================================

(define handle_car
		(lambda () 

			(string-append (applic-prolog "car_code" "end_car_code")

				"\ncar_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notAPair\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_PAIR\n"
				"\tjne .notAPair\n"
				;"\tCAR r10\n"
				"\tDATA_UPPER r10\n"
				"\tadd r10, start_of_data\n"
				"\tmov rax, r10\n"
				"\tjmp .done\n\n"
				".notAPair:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_car_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [car], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR CAR EXPRESSION =============================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR CDR EXPRESSION ====================================================
;=========================================================================================================================================

(define handle_cdr
		(lambda () 

			(string-append (applic-prolog "cdr_code" "end_cdr_code")

				"\ncdr_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notAPair\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_PAIR\n"
				"\tjne .notAPair\n"
				;"\tCDR rax\n"
				"\tDATA_LOWER r10\n"
				"\tadd r10, start_of_data\n"
				"\tmov rax, r10\n"
				"\tjmp .done\n\n"
				".notAPair:\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_cdr_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [cdr], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR CDR EXPRESSION =============================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR EQ? EXPRESSION ====================================================
;=========================================================================================================================================

(define handle_eq? 
		(lambda () 
			(string-append (applic-prolog "eq?_code" "end_eq?_code")

				"eq?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov r10, qword [rbp + 8*4]\n"
				"\tmov r9, qword [rbp + 8*5]\n"
				"\tmov rbx, [r10]\n"
				"\tmov rcx, [r9]\n"
				"\tTYPE rbx\n"
				"\tTYPE rcx\n"
				"\tcmp rbx, rcx\n"
				"\tjne .retFalse\n"
				"\tmov rbx, [r10]\n"
				"\tmov rcx, [r9]\n"
				"\tTYPE rbx\n"
				"\tTYPE rcx\n"
				"\tcmp rbx, T_SYMBOL\n"
				"\tje .checkSymbols\n"
				"\tmov rbx, [r10]\n"
				"\tmov rcx, [r9]\n"
				"\tcmp rbx, rcx\n"
				"\tje .retTrue\n"
				"\tjmp .retFalse\n"

				".checkSymbols:\n\n"
				"\tmov r10, [r10]\n"
				"\tDATA r10\n"
				"\tpush r10\n"
				"\tpush SymbolTable\n"
				"\tcall findSymbol\n"
				"\tadd rsp, 2*8\n"
				"\tmov r14, rax\n"
				"\tmov r9, [r9]\n"
				"\tDATA r9\n"
				"\tpush r9\n"
				"\tpush SymbolTable\n"
				"\tcall findSymbol\n"
				"\tadd rsp, 2*8\n"
				"\tcmp rax, r14\n"
				"\tje .retTrue\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .doneEq?\n"
				".retTrue:\n"
				"\tmov rax, sobTrue\n"
				"\tjmp .doneEq?\n"
				".retFalse:\n\n"
				"\tmov rax, sobFalse\n"
				".doneEq?:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_eq?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [eq?], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR STRING->SYMBOL EXPRESSION ==================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR SYMBOL->STRING EXPRESSION =========================================
;=========================================================================================================================================

(define handle_symbol->string 
		(lambda () 
			(string-append (applic-prolog "symbolToString_code" "end_symbolToString_code")

				"symbolToString_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tDATA r10\n"
				"\tpush r10\n"
				"\tpush SymbolTable\n"
				"\tcall findSymbol\n"
				"\tadd rsp, 2*8\n"
				"\tmov rax, [rax]\n"
				"\tDATA rax\n"
				".doneSymbolToString:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_symbolToString_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [symbolToString], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR SYMBOL->STRING EXPRESSION ==================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR STRING->SYMBOL EXPRESSION =========================================
;=========================================================================================================================================

(define handle_string->symbol 
		(lambda () 
			(string-append (applic-prolog "stringToSymbol_code" "end_stringToSymbol_code")

				"stringToSymbol_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov r10, qword [rbp + 8*4]\n"
				"\tmov rdi, 8\n"
			    "\tcall malloc\n"
			    "\tmov r9, rax\n"
			    "\tmov r8, r10\n"
			    "\tshl r8, 4\n"
			    "\tor r8, T_SYMBOL\n"
			    "\tmov qword [rax], r8\n"
			    "\tpush rax\n"
			    "\tpush SymbolTable\n"
			    "\tcall addSymbol\n"
			    "\tadd rsp, 2*8\n"
				"\tmov rax, r9\n"
				;"\tDATA rax\n"
				".doneStringToSymbol:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_stringToSymbol_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [stringToSymbol], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR STRING->SYMBOL EXPRESSION ==================================
;=========================================================================================================================================







;=========================================================================================================================================
;======================================================= FUNCTIONS FOR SYMBOL? EXPRESSION ================================================
;=========================================================================================================================================

(define handle_symbol? 
		(lambda () 
			(string-append (applic-prolog "symbol?_code" "end_symbol?_code")

				"symbol?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_SYMBOL\n"
				"\tje trueSymbol?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp doneSymbol?\n\n"
				"trueSymbol?:\n"
				"\tmov rax, sobTrue\n\n"
				"doneSymbol?:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_symbol?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [symbol?], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR SYMBOL? EXPRESSION =========================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR ZERO? EXPRESSION ==================================================
;=========================================================================================================================================

(define handle_zero? 
		(lambda () 

			(string-append (applic-prolog "zero?_code" "end_zero?_code")


				"\nzero?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notANumber\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .chechIfZero\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".chechIfZero:\n"
				"\tcmp r10, MAKE_LITERAL(T_INTEGER, 0)\n"
				"\tje .isZero\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".isZero:\n"
				"\tmov rax, sobTrue\n"
				"\tjmp .done\n"
				".notANumber:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_zero?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [zero?], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR ZERO? EXPRESSION ===========================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR NOT EXPRESSION ====================================================
;=========================================================================================================================================

(define handle_not 
		(lambda () 

			(string-append (applic-prolog "not_code" "end_not_code")

				"\nnot_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_BOOL\n"
				"\tjne .retFalse\n"
				"\tmov rbx, rax\n"
				"\tcmp rbx, sobFalse\n"
				"\tjne .retFalse\n"
				"\tmov rax, sobTrue\n"
				"\tjmp .done\n\n"
				".retFalse:\n"
				"\tmov rax, sobFalse\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_not_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [not], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR NOT EXPRESSION =============================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR VECTOR? EXPRESSION ================================================
;=========================================================================================================================================

(define handle_vector? 
		(lambda () 

			(string-append (applic-prolog "vector?_code" "end_vector?_code")

				"\nvector?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_VECTOR\n"
				"\tje .trueVector?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".trueVector?:\n"
				"\tmov rax, sobTrue\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_vector?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [vector?], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR VECTOR? EXPRESSION =========================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR STRING? EXPRESSION ================================================
;=========================================================================================================================================

(define handle_string? 
		(lambda () 

			(string-append (applic-prolog "string?_code" "end_string?_code")

				"\nstring?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_STRING\n"
				"\tje .trueString?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".trueString?:\n"
				"\tmov rax, sobTrue\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_string?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [string?], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR STRING? EXPRESSION =========================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR CHAR? EXPRESSION ==================================================
;=========================================================================================================================================

(define handle_char? 
		(lambda () 

			(string-append (applic-prolog "char?_code" "end_char?_code")

				"\nchar?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_CHAR\n"
				"\tje .trueChar?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".trueChar?:\n"
				"\tmov rax, sobTrue\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_char?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [char?], rax\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR CHAR? EXPRESSION ===========================================
;=========================================================================================================================================

;=========================================================================================================================================
;======================================================= FUNCTIONS FOR PROCEDURE? EXPRESSION =============================================
;=========================================================================================================================================

(define handle_procedure?
		(lambda ()

			(string-append (applic-prolog "procedure?_code" "end_procedure?_code")

				"\nprocedure?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_CLOSURE\n"
				"\tje .trueProcedure?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".trueProcedure?:\n"
				"\tmov rax, sobTrue\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_procedure?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [procedure?], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR PAIR? EXPRESSION ===========================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR PAIR? EXPRESSION ==================================================
;=========================================================================================================================================

(define handle_pair?
		(lambda ()

			(string-append (applic-prolog "pair?_code" "end_pair?_code")

				"\npair?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_PAIR\n"
				"\tje .truePair?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".truePair?:\n"
				"\tmov rax, sobTrue\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_pair?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [pair?], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR PAIR? EXPRESSION ===========================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR BOOLEAN? EXPRESSION ===============================================
;=========================================================================================================================================


(define handle_boolean?
		(lambda ()

			(string-append (applic-prolog "boolean?_code" "end_boolean?_code")

				"\nboolean?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_BOOL\n"
				"\tje .trueBoolean?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".trueBoolean?:\n"
				"\tmov rax, sobTrue\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_boolean?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [boolean?], rax\n\n")))



;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR BOOLEAN? EXPRESSION ========================================
;=========================================================================================================================================

;=========================================================================================================================================
;======================================================= FUNCTIONS FOR RATIONAL? EXPRESSION ===============================================
;=========================================================================================================================================

(define handle_rational?
		(lambda ()

			(string-append (applic-prolog "rational?_code" "end_rational?_code")
			
				"\nrational?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .trueRational?\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .trueRational?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".trueRational?:\n"
				"\tmov rax, sobTrue\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_rational?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [rational?], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR RATIONAL? EXPRESSION ========================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR INTEGER? EXPRESSION ===============================================
;=========================================================================================================================================

(define handle_integer?
		(lambda ()

			(string-append (applic-prolog "integer?_code" "end_integer?_code")

				"\ninteger?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .trueInteger?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".trueInteger?:\n"
				"\tmov rax, sobTrue\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_integer?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [integer?], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR INTEGER? EXPRESSION ========================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR NULL? EXPRESSION ==================================================
;=========================================================================================================================================

(define handle_null?
		(lambda ()

			(string-append (applic-prolog "null?_code" "end_null?_code")

				"\nnull?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_NIL\n"
				"\tje .trueNull?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".trueNull?:\n"
				"\tmov rax, sobTrue\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_null?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [null?], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR NULL? EXPRESSION ===========================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR NUMBER? EXPRESSION ================================================
;=========================================================================================================================================

(define handle_number?
		(lambda ()

			(string-append (applic-prolog "number?_code" "end_number?_code")

				"\nnumber?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov r10, [rax]\n"
				"\tmov rbx, r10\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .trueNumber?\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .trueNumber?\n"
				"\tmov rax, sobFalse\n"
				"\tjmp .done\n\n"
				".trueNumber?:\n"
				"\tmov rax, sobTrue\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, sobVoid\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_number?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [number?], rax\n\n")))



;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR NUMBER? EXPRESSION =========================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR DEF EXPRESSION ====================================================
;=========================================================================================================================================

(define handle_define
	(lambda (def-exp depth const-table global-env)
		;(display "def-exp handle_define: ") (display def-exp) (newline)
		;(display "global-env handle_define: ") (display global-env) (newline) 
		(let ((free-var (find-var-in-global-env (cadar def-exp) global-env)))
			;(display "free-var handle_define: ") (display free-var) (newline)
				(string-append 
					(code-gen (cadr def-exp) depth const-table global-env)
					"\tmov rbx, " free-var "\n" 
					"\tmov r10, [rax]\n" 
					"\tmov qword [rbx], r10\n"
					"\tmov rax, sobVoid\n\n"))))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR DEF EXPRESSION =============================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR SEQ EXPRESSION ====================================================
;=========================================================================================================================================

(define handle_seq
  (lambda (seq-exp depth const-table global-env)
    (fold-left (lambda (res exp) (string-append res (code-gen exp depth const-table global-env))) "" (cadr seq-exp))
    ))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR SEQ EXPRESSION =============================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR OR EXPRESSION =====================================================
;=========================================================================================================================================


(define handle_or
		(lambda (or-exp depth end-label const-table global-env)	
				(if (null? or-exp) 
					(string-append end-label ":\n\n")
					(string-append 
						(code-gen (car or-exp) depth const-table global-env)
						"\tmov r10, [rax]\n" 
						"\tmov rbx, r10\n"
						"\tTYPE rbx\n"
						"\tcmp rbx, SOB_FALSE\n" 
						"\tjne " end-label "\n"
						(handle_or (cdr or-exp) depth end-label const-table global-env)))))

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
		(lambda (if-exp depth const-table global-env)
			;(display "if-exp in handle_if: ") (display if-exp) (newline) 
			;(display "cadr if-exp in handle_if: ") (display (code-gen (cadr if-exp) const-table)) (newline)
			;(display "caddr if-exp in handle_if: ") (display (code-gen (caddr if-exp) const-table)) (newline)
			;(display "cadddr if-exp in handle_if: ") (display (code-gen (cadddr if-exp) const-table)) (newline) (newline)
			(let ((dif-label (make-dif-label))
				  (end-label (make-end-label)))
				 (string-append 
					(code-gen (cadr if-exp) depth const-table global-env)
					"\tmov r10, [rax]\n" 
					"\tcmp r10, SOB_FALSE\n" 
					"\tje " dif-label "\n"
					(code-gen (caddr if-exp) depth const-table global-env) "\n"
					"\tjmp " end-label "\n"
					dif-label ":\n"
					"\t" (code-gen (cadddr if-exp) depth const-table global-env) "\n"
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
			  ((and (pair? lst) (not (pair? (cdr lst)))) (append (list lst) (list (car lst)) (list (cdr lst))))
			  (else (append (list (car lst)) (list (cdr lst)) (break-list-to-components (cdr lst)))))))

(define expand-const-table
	(lambda (lst)
		;(display "expand-const-table") (newline) 
		;(display "expand-const-table lst: ") (display lst) (newline) (newline)
		(cond ((and (list? lst) (> (length lst) 0) (vector? (car lst))) 
					(append (vector->list (car lst)) (expand-vector (vector->list (car lst))) (list (car lst)) (expand-const-table (cdr lst))))		
			  ((and (list? lst) (> (length lst) 0) (or (list? (car lst)) (pair? (car lst)))) (append (break-list-to-components (car lst)) (list (car lst)) (expand-const-table (cdr lst))))
			  ((and (list? lst) (> (length lst) 0) (not (list? (car lst))) (number? (car lst)) (not (integer? (car lst)))) (append (list (car lst)) (list (numerator (car lst))) (list (denominator (car lst))) (expand-const-table (cdr lst))))
			  ((and (list? lst) (> (length lst) 0) (not (list? (car lst)))) (cons (car lst) (expand-const-table (cdr lst))))
			  
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
		(let* ((con-list (reverse (remove-duplicates (reverse (append (list (void) #f #t '()) (expand-const-table (make_const_table input-file)))))))
			  (strings-of-symbols-list (map add-strings-of-symbols con-list)))
			  (remove-duplicates (append con-list strings-of-symbols-list)))))

(define create_const_for_assembly
	(lambda (const-table table-in-pairs num)
		;(display "create_const_for_assembly const-table : ") (display const-table) (newline)
		;(display "create_const_for_assembly table-in-pairs: ") (display table-in-pairs) (newline) (newline)
		(set! num (+ num 1))
		(cond ((null? const-table) (string-append "sobUndef:" "\n" "\tdq SOB_UNDEFINED\n\n"))
			  ((integer? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL (T_INTEGER, " (number->string (car const-table)) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((number? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL_FRACTION (" (find-const-in-pairs (numerator (car const-table)) table-in-pairs) ", " (find-const-in-pairs (denominator (car const-table)) table-in-pairs) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((boolean? (car const-table)) (if (equal? (car const-table) #t)
			  					  (string-append "sobTrue:" "\n" "\tdq SOB_TRUE\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num))
			  					  (string-append "sobFalse:" "\n" "\tdq SOB_FALSE\n" (create_const_for_assembly (cdr const-table) table-in-pairs num))))
			  ((char? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":\n" "\tdq MAKE_LITERAL(T_CHAR, " (number->string (char->integer (car const-table))) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((symbol? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq SOB_UNDEFINED\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((string? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tMAKE_LITERAL_STRING " (check_for_special_chars (map char->integer (string->list (car const-table)))) "\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((null? (car const-table)) 
			  		(string-append "sobNil:" "\n" "\tdq SOB_NIL\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((vector? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tMAKE_LITERAL_VECTOR " (trim-last-comma (get-components-from-pairs (car const-table) table-in-pairs)) "\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((and (list? (car const-table)) (> (length (car const-table)) 1) (list? (cadr (car const-table))))
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL_PAIR (" (find-const-in-pairs (caar const-table) table-in-pairs) ", " (find-const-in-pairs (cadr (car const-table)) table-in-pairs) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((or (list? (car const-table)) (pair? (car const-table))) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL_PAIR (" (find-const-in-pairs (caar const-table) table-in-pairs) ", " (find-const-in-pairs (cdr (car const-table)) table-in-pairs) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  (else (string-append "sobVoid:" "\n" "\tdq SOB_VOID\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
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
			  ((> (length table-in-pairs) 0) (find-const-in-pairs const (cdr table-in-pairs))))
		))

(define get-components-from-pairs
	(lambda (vec table-in-pairs)
		;(display "get-components-from-pairs vec: ") (display vec) (newline) 
		;(display "get-components-from-pairs table-in-pairs: ") (display table-in-pairs) (newline) (newline)
		(cond ((null? table-in-pairs) "")
			  ((member (cadar table-in-pairs) (vector->list vec)) (string-append (caar table-in-pairs) ", " (get-components-from-pairs vec (cdr table-in-pairs))))
			  (else (get-components-from-pairs vec (cdr table-in-pairs))))))


(define pairs_of_name_and_object
	(lambda (con num)
		;(display "pairs_of_name_and_object con : ") (display con) (newline)
		(cond ((integer? con) (if (negative? con)
								  (list (string-append "sobNegInt" (number->string (abs con))) con)
								  (list (string-append "sobInt" (number->string con)) con)))
			  ((number? con) (if (negative? con) 
			  					 (list (string-append "sobNegFrac" (number->string (abs (numerator con))) "_" (number->string (denominator con))) con)
			  					 (list (string-append "sobFrac" (number->string (numerator con)) "_" (number->string (denominator con))) con)))
			  ((boolean? con) (if (equal? con #t)
			  					  (list "sobTrue" con)
			  					  (list "sobFalse" con)))
			  ((char? con) (list (string-append "sobChar" (number->string num)) con))
			  ((string? con) (list (string-append "sobString" (number->string num)) con))
			  ((null? con) (list "sobNil" '()))
			  ((vector? con) (list (string-append "sobVector" (number->string num)) con))
			  ((symbol? con) (list (string-append "sobSymbol" (symbol->string con)) con))
			  ((or (list? con) (pair? con)) (list (string-append "sobPair" (number->string num)) con))
			  (else (list "sobVoid" con))
			  )))


(define check_for_special_chars
	(lambda (char_list)
		(fold-left
			(lambda (acc charAsNum)
				(cond
					((= charAsNum 0) (if (null? (string->list acc))
										 (string-append acc "CHAR_NUL")
										 (string-append acc ", CHAR_NUL")))
					((= charAsNum 9) (if (null? (string->list acc))
										 (string-append acc "CHAR_TAB")
										 (string-append acc ", CHAR_TAB")))
					((= charAsNum 10) (if (null? (string->list acc))
										 (string-append acc "CHAR_NEWLINE")
										 (string-append acc ", CHAR_NEWLINE")))
					((= charAsNum 12) (if (null? (string->list acc))
										 (string-append acc "CHAR_PAGE")
										 (string-append acc ", CHAR_PAGE")))
					((= charAsNum 13) (if (null? (string->list acc))
										 (string-append acc "CHAR_RETURN")
										 (string-append acc ", CHAR_RETURN")))
					((= charAsNum 32) (if (null? (string->list acc))
										  (string-append acc "CHAR_SPACE")
										  (string-append acc ", CHAR_SPACE")))
					(else (cond ((null? (string->list acc)) (string-append acc "\"" (string (integer->char charAsNum)) "\""))
							    ((= (char->integer (car (reverse (string->list acc)))) 34) (string-append (list->string (reverse (cdr (reverse (string->list acc))))) (string (integer->char charAsNum)) "\""))
								(else (string-append acc ", \"" (string (integer->char charAsNum)) "\""))))))
			"\"\""
			char_list)))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR CONSTANT TABLE =============================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR SYMBOL TABLE ======================================================
;=========================================================================================================================================

(define get_symbols
	(lambda (exp)
		(cond ((and (list? exp) (null? exp)) exp)
			  ((and (list? exp) (equal? (car exp) 'const) (symbol? (cadr exp))) (list (cadr exp)))
			  ((and (list? exp) (list? (car exp))) (append (get_symbols (car exp)) (get_symbols (cdr exp))))
			  ((and (list? exp) (> (length exp) 0) (not (list? (car exp)))) (get_symbols (cdr exp)))
			  ((and (list? exp) (vector? (car exp))) (append (get_symbols (vector->list (car exp))) (get_symbols (cdr exp)))))))



;(define create-symbol-table
;	(lambda (input-file)
;		(map (lambda (sym) (list (string-append "sobSymbol_" (check_for_special_symbols (symbol->string sym))) sym)) (remove-duplicates (get_symbols input-file)))))

;(define create-string-of-symbols
;		(lambda (sym-table)
;			(if (null? sym-table) 
;				""
;				(string-append (caar sym-table) ":" "\n" "\tMAKE_LITERAL_STRING \""  (symbol->string (cadar sym-table)) "\"\n\n" (create-string-of-symbols (cdr sym-table))))))

(define add-strings-of-symbols
		(lambda (sym)
			(if (symbol? sym)
				(symbol->string sym)
				sym)))

(define create-symbol-table-for-assembly
		(lambda (const-table table-in-pairs)
			(cond ((null? const-table) "")
				  ((symbol? (car const-table)) (string-append  "\tmov rdi, 8\n"
															   "\tcall malloc\n"
															   "\tmov r9, " (find-const-in-pairs (symbol->string (car const-table)) table-in-pairs) "\n"
															   "\tshl r9, 4\n"
															   "\tor r9, T_SYMBOL\n"
															   "\tmov r8, " (find-const-in-pairs (car const-table) table-in-pairs) "\n"
															   "\tmov qword [r8], r9\n"
															   "\tshl r8, 4\n"
															   "\tor r8, T_SYMBOL\n"
															   "\tmov qword [rax], r8\n"
															   "\tpush rax\n"
															   "\tpush SymbolTable\n"
															   "\tcall addSymbol\n"
															   "\tadd rsp, 2*8\n"
															   (create-symbol-table-for-assembly (cdr const-table) table-in-pairs)))
					(else (create-symbol-table-for-assembly (cdr const-table) table-in-pairs)))))


(define check_for_special_symbols
		(lambda (sym)
				(cond 
				  ((member #\! (string->list (car sym))) (list (list->string (substq #\B #\! (string->list (car sym)))) (cadr sym)))
				  ((member #\< (string->list (car sym))) (list (list->string (substq #\L #\< (string->list (car sym)))) (cadr sym)))
				  ((member #\> (string->list (car sym))) (list (list->string (substq #\G #\> (string->list (car sym)))) (cadr sym)))
				  ((member #\= (string->list (car sym))) (list (list->string (substq #\E #\= (string->list (car sym)))) (cadr sym)))
				  ((member #\+ (string->list (car sym))) (list (list->string (substq #\P #\+ (string->list (car sym)))) (cadr sym)))
				  ((member #\/ (string->list (car sym))) (list (list->string (substq #\D #\/ (string->list (car sym)))) (cadr sym)))
				  ((member #\* (string->list (car sym))) (list (list->string (substq #\M #\* (string->list (car sym)))) (cadr sym)))
				  ((member #\- (string->list (car sym))) (list (list->string (substq #\S #\- (string->list (car sym)))) (cadr sym)))
				  (else (list (car sym) (cadr sym))))))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR SYMBOL TABLE ===============================================
;=========================================================================================================================================

;=========================================================================================================================================
;======================================================= FUNCTIONS FOR GLOBAL ENV ========================================================
;=========================================================================================================================================

(define make_global_env
	(lambda (exp)
		(cond ((and (list? exp) (null? exp)) exp)
			  ((and (list? exp) (equal? (car exp) 'fvar) (not (in-primitive-procedures? (cadr exp)))) (list (cadr exp)))
			  ((and (list? exp) (list? (car exp))) (append (make_global_env (car exp)) (make_global_env (cdr exp))))
			  ((and (list? exp) (> (length exp) 0) (not (list? (car exp)))) (make_global_env (cdr exp))))))

(define global_env
	(lambda (input-file)
		(remove-duplicates (make_global_env input-file))))

(define create_global_env_for_assembly
		(lambda (global-env-table-as-pairs)
			;(display "create_global_env_for_assembly: ") (display global-env-table-as-pairs) (newline) (newline)
			(cond  ((null? global-env-table-as-pairs) "") 
				   ((in-primitive-procedures? (cadar global-env-table-as-pairs)) (create_global_env_for_assembly (cdr global-env-table-as-pairs)))
				   (else (string-append (caar global-env-table-as-pairs) ":\n" "\tdq SOB_UNDEFINED\n\n" (create_global_env_for_assembly (cdr global-env-table-as-pairs)))))))

(define find-var-in-global-env
	(lambda (var global-env)
		;(display "var find-var-in-global-env: ") (display var) (newline)
		;(display "global-env find-var-in-global-env: ") (display global-env) (newline) 
		;(display "cadar global-env find-var-in-global-env: ") (display (string? (cadar global-env))) (newline)(newline)
		(if (equal? var (cadar global-env))
			(caar global-env)
			(find-var-in-global-env var (cdr global-env)))))

(define pairs_of_label_and_name
	(lambda (var)
		;(display "var pairs_of_label_and_name: ") (display var) (newline)
		(let ((name (symbol->string var)))
			(cond ((equal? ">" name) (list "greaterThan" var))
				  ((equal? "<" name) (list "lessThan" var))
				  ((equal? "=" name) (list "equal" var))
				  ((equal? "+" name) (list "plus" var))
				  ((equal? "/" name) (list "divide" var))
				  ((equal? "*" name) (list "multiply" var))
				  ((equal? "-" name) (list "subtract" var))


				  ((equal? "char->integer" name) (list "charToInteger" var))
				  ((equal? "integer->char" name) (list "integerToChar" var))
				  ((equal? "make-string" name) (list "makeString" var))
				  ((equal? "make-vector" name) (list "makeVector" var))
				  ((equal? "set-car!" name) (list "setCar" var))
				  ((equal? "set-cdr!" name) (list "setCdr" var))

				  ((equal? "string-ref" name) (list "stringRef" var))
				  ((equal? "string-set!" name) (list "stringSet" var))
				  ((equal? "string-length" name) (list "stringLength" var))
				  ((equal? "make-string" name) (list "makeString" var))
				  ((equal? "make-vector" name) (list "makeVector" var))
				  ((equal? "string->symbol" name) (list "stringToSymbol" var))
				  ((equal? "symbol->string" name) (list "symbolToString" var))

				  ((equal? "vector-length" name) (list "vectorLength" var))
				  ((equal? "vector-ref" name) (list "vectorRef" var))
				  ((equal? "vector-set!" name) (list "vectorSet" var))
				  (else (list name var))))))

(define init-primitives
	(lambda (primitive-procedures global-env-as-pairs)
		;(display "init-primitives: ") (display (primitive-procedures)) (newline) (newline)
		(if (= 0 (length primitive-procedures))
			""
			(string-append (find-var-in-global-env (car primitive-procedures) global-env-as-pairs) ":\n" "\tdq SOB_UNDEFINED\n\n" (init-primitives (cdr primitive-procedures) global-env-as-pairs)))))
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

(define primitive-procedures
		(list 'append 'apply '< '= '> '+ '/ '* '- 'boolean? 'car 'cdr 'char->integer 'char? 'cons 'denominator
			'eq? 'integer? 'integer->char 'list 'make-string 'make-vector 'map 'not
			'null? 'number? 'numerator 'pair? 'procedure? 'rational? 'remainder 'set-car! 'set-cdr!
			'string-length 'string-ref 'string-set! 'string->symbol 'string? 'symbol? 'symbol->string
			'vector 'vector-length 'vector-ref 'vector-set! 'vector? 'zero?))



(define in-primitive-procedures?
	(lambda (exp)
		(member exp '(append apply < = > + / * - boolean? car cdr char->integer char? cons denominator
			eq? integer? integer->char list make-string make-vector map not 
			null? number? numerator pair? procedure? rational? remainder set-car! set-cdr!
			string-length string-ref string-set! string->symbol string? symbol? symbol->string
			vector vector-length vector-ref vector-set! vector? zero?))))

;=========================================================================================================================================
;======================================================= END OF GENERAL UTILITY FUNCTIONS ================================================
;=========================================================================================================================================
