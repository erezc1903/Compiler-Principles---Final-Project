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
	  	(display "pe in code-gen: ") (display pe) (newline) (newline)
	      (string-append  
	      	(cond ((tagged-by? pe 'const) (string-append "\t; codegen for const start\n\tmov rax, qword [" (find-const-in-pairs (cadr pe) const-table) "]\n\t;code gen for constant end\n"))
			       ((tagged-by? pe 'if3) (handle_if pe depth const-table global-env))
			       ((tagged-by? pe 'seq) (handle_seq pe depth const-table global-env))
			       ((tagged-by? pe 'or) (handle_or (cadr pe) depth (make-end-label-for-or) const-table global-env))
			       ((tagged-by? pe 'def) (handle_define (cdr pe) depth const-table global-env))
			       ((tagged-by? pe 'applic) (handle_applic pe depth const-table global-env))
			       ((tagged-by? pe 'lambda-simple) (handle_lambda_simple (cadr pe) (caddr pe) depth const-table global-env))
			       ;((tagged-by? pe 'tc-applic) (handle_tc_applic pe))
			       ;((tagged-by? pe 'lambda-opt) (handle_lambda_opt pe))
			       ((tagged-by? pe 'pvar) (handle_pvar pe))
			       ((tagged-by? pe 'bvar) (handle_bvar pe))
			       ((tagged-by? pe 'fvar) (handle_fvar (cadr pe) depth const-table global-env))
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
			  (global-env (append primitive-procedures (global_env input)))
			  (global-env-as-pairs (map pairs_of_label_and_name global-env)))
			  ;(symbol-table (symbol_table input))

			;(display "global-env-as-pairs: ") (display global-env-as-pairs) (newline) (newline)
			(display "input: ") (display input) (newline) (newline)
			;(display "const-table-as-list-of-pairs: ") (display const-table-as-list-of-pairs) (newline) (newline)


			(fprintf out-port "%include \"scheme.s\"\n\n") 
			
			(fprintf out-port "section .bss\n\n") 
			
			(fprintf out-port "section .data\n\n") 

			;(fprintf out-port "globel_env:\n\n")

			(fprintf out-port  (init-primitives primitive-procedures global-env-as-pairs))

			(fprintf out-port (create_global_env_for_assembly global-env-as-pairs))

			(fprintf out-port (create_const_for_assembly const-table const-table-as-list-of-pairs 0))

			(fprintf out-port "\n\nsection .text\n\n")
			(fprintf out-port "\textern exit, printf, scanf, malloc\n\n")
			(fprintf out-port "\tglobal main\n\n")

			(fprintf out-port "main:\n\n")

			(fprintf out-port "; =============================== PRIMITIVE FUNCTIONS =========================\n")
			(fprintf out-port (creat-primitive-procedures global-env-as-pairs))
			(fprintf out-port "; =============================== PRIMITIVE FUNCTIONS =========================\n")

			(fprintf out-port "\nstart_of_instructions:\n\n")
			(fprintf out-port "\tpush rbp\n") 
			(fprintf out-port "\tmov rbp, rsp\n")
			;(fprintf out-port "\tpush 0\n")

			(for-each (lambda (pe) 
					(fprintf out-port 
						(string-append "\n; start\n" (code-gen pe 0 const-table-as-list-of-pairs global-env-as-pairs)
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
				(handle_pair? global-env-as-pairs)
				(handle_boolean? global-env-as-pairs)
				(handle_integer? global-env-as-pairs)
				(handle_null? global-env-as-pairs)
				(handle_number? global-env-as-pairs)
				(handle_char? global-env-as-pairs)
				(handle_string? global-env-as-pairs)
				;(handle_symbol?)
				(handle_vector? global-env-as-pairs)
				(handle_not global-env-as-pairs)
				(handle_rational? global-env-as-pairs)
				(handle_zero? global-env-as-pairs)
				(handle_car global-env-as-pairs)
				(handle_cdr global-env-as-pairs)
				(handle_cons global-env-as-pairs)
				(handle_numerator global-env-as-pairs)
				(handle_denominator global-env-as-pairs)
				(handle_integer->char global-env-as-pairs)
				(handle_char->integer global-env-as-pairs)
				;(handle_plus global-env-as-pairs)
				(handle_greater_than global-env-as-pairs)
				(handle_less_than global-env-as-pairs)
				(handle_equal global-env-as-pairs)
				 "")))




;=========================================================================================================================================
;======================================================= FUNCTIONS FOR APP EXPRESSION ====================================================
;=========================================================================================================================================

(define handle_applic
		(lambda (app-exp depth const-table global-env)
			(display "handle_applic app: ") (display app-exp) (newline)
			(display "handle_applic depth: ") (display depth) (newline) (newline)
			(let ((app (cadr app-exp))
				  (args (caddr app-exp))
				  (not-a-closure-label (make-not-a-closure-label))
				  (done-closure-label (make-done-closure-label)))
				  (display "app: ") (display app) (newline)
				  ;(cond ((in-primitive-procedures? (cadr app)) (string-append (push-args (reverse args) (length args) (+ depth 1) const-table global-env)
					 ; 															"\tpush " (number->string (length args)) "\n"
					 ; 															"\tmov rax, qword [" (find-var-in-global-env (cadr app) global-env) "]\n" 
					 ; 															"\tmov rcx, rax\n"
					 ; 															"\tTYPE rcx\n"
					 ; 															"\tcmp rcx, T_CLOSURE\n"
					 ; 															"\tjne " not-a-closure-label "\n"
					 ; 															"\tmov rbx, rax\n"
						;  														"\tCLOSURE_ENV rbx\n"
						;  														"\tpush rbx\n"
						;  														"\tCLOSURE_CODE rax\n"
						;  														"\tcall rax\n"
						;  														"\tjmp " done-closure-label "\n"
						;  														not-a-closure-label ":\n\n"
						;  														"\tmov rax, SOB_VOID\n"
						;  														done-closure-label ":\n\n"
						;  														"\tadd rsp, 8*" (number->string (+ 1 (length args))) "\n\n"))
					  ;((equal? (car app) 'lambda-simple) 
					  	(string-append "; start of applic of lambda-simple code: \n\n"
										(push-args (reverse args) (length args) depth const-table global-env)
										"\tpush " (number->string (length args)) "\n"
										(code-gen app depth const-table global-env)
										"\tmov rcx, rax\n"
										"\tTYPE rcx\n"
										"\tcmp rcx, T_CLOSURE\n"
										"\tjne " not-a-closure-label "\n"
										"\tmov rbx, rax\n"
										"\tCLOSURE_ENV rbx\n"
										"\tpush rbx\n"
										"\tCLOSURE_CODE rax\n"
										"\tcall rax\n"
										"\tadd rsp, 8*1\n"
										"\tjmp " done-closure-label "\n"
										not-a-closure-label ":\n\n"
										"\tmov rax, SOB_VOID\n"
										done-closure-label ":\n\n"
										"\tadd rsp, 8*" (number->string (+ 1 (length args))) "\n\n"
										"; end of applic of lambda-simple code: \n\n"))
					  ;((equal? app-exp '(fvar >)) (handle_greater_then (length (cdr args)) (cdr args) depth const-table global-env))

					))



; we assume that the arg list comes reversed
(define push-args 
		(lambda (args numberOfArgs depth const-table global-env)
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
;======================================================= FUNCTIONS FOR LAMBDA SIMPLE EXPRESSION ==========================================
;=========================================================================================================================================


(define handle_lambda_simple
		(lambda (params body depth const-table global-env)
			(display "handle_lambda_simple params: ") (display params) (newline)
			(display "handle_lambda_simple body: ") (display body) (newline)
			(display "handle_lambda_simple depth: ") (display depth) (newline)
			(let* ((body-label (make-body-label-for-lambda-simple))
				  (copy-args-label (make-copy-args-label-for-lambda-simple))
				  (copy-env-label (make-copy-env-label-for-lambda-simple))
				  (no-args-label (make-no-args-label-for-lambda-simple))
				  (done-copy-args (make-done-copying-args-label-for-lambda-simple))
				  (make-closure-label (make-make-closure-label-for-lambda-simple))
				  (end-label (make-end-label-for-lambda-simple))
				  (extended-env
				  	 (string-append 
				  	 			"\txor rax, rax\n"
				 				"\tmov rdi, " (number->string (* 8 (length params))) "\n"
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
				 				"\tcmp r10, " (number->string depth) "\n"
				 				"\tje " make-closure-label "\n" 
				 				"\tlea r12, [rbp + 8*2]\n"
				 				"\tmov r12, [r12 + 8*r10]\n"
				 				"\tmov [rbx + 8*r15], r12\n"
				 				"\tinc r10\n"
				 				"\tinc r15\n"
				 				"\tjmp " copy-env-label "\n\n"))
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
							    (code-gen body (+ depth 1) const-table global-env)
							    ;"\tmov rsp, rbp\n"
								"\tpop rbp\n"
								"\tret\n\n"

							end-label":\n"
							"\tmov rax, [rax] ; rax now hold the closure object \n\n"

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
;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR LAMBDA SIMPLE EXPRESSION ===================================
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
;======================================================= FUNCTIONS FOR EQUAL EXPRESSION ==================================================
;=========================================================================================================================================

(define handle_equal
		(lambda (global-env) 

			(string-append (applic-prolog "equal_code" "end_equal_code")

				"equal_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .check_equal\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
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
				"\tje .doneCheckEQ\n"

				".check_equal:\n\n"
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				"\tmov r9, qword [rbp + 8*3]\n"
				"\tdec r9\n"

				".check_equal_loop:\n\n"
				"\tcmp rcx,r9\n"
				"\tje .doneCheckEQ\n\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
				"\tDATA rbx\n"
				"\tmov rdx, qword [rbp + 8*(5 + rcx)]\n"
				"\tDATA rdx\n"
				"\tcmp rbx, rdx\n"
				"\tjne .retFalse\n"
				"\tinc rcx\n"
				"\tjmp .check_equal_loop\n"

				".retFalse:\n\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n"

				".doneCheckEQ:\n\n"
				"\tmov rax, SOB_TRUE\n"
				"\tjmp .done\n"

				".badArgs:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "greater_than_code" "end_greater_than_code")

				"greater_than_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .check_greater_than\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
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
				"\tje .doneCheckGT\n"

				".check_greater_than:\n\n"
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				"\tmov r9, qword [rbp + 8*3]\n"
				"\tdec r9\n"

				".check_greater_than_loop:\n\n"
				"\tcmp rcx,r9\n"
				"\tje .doneCheckGT\n\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
				"\tDATA rbx\n"
				"\tmov rdx, qword [rbp + 8*(5 + rcx)]\n"
				"\tDATA rdx\n"
				"\tcmp rbx, rdx\n"
				"\tjle .retFalse\n"
				"\tinc rcx\n"
				"\tjmp .check_greater_than_loop\n"

				".retFalse:\n\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n"

				".doneCheckGT:\n\n"
				"\tmov rax, SOB_TRUE\n"
				"\tjmp .done\n"

				".badArgs:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "less_than_code" "end_less_than_code")

				"less_than_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .check_less_than\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
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
				"\tje .doneCheckLT\n"

				".check_less_than:\n\n"
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				"\tmov r9, qword [rbp + 8*3]\n"
				"\tdec r9\n"

				".check_less_than_loop:\n\n"
				"\tcmp rcx,r9\n"
				"\tje .doneCheckLT\n\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
				"\tDATA rbx\n"
				"\tmov rdx, qword [rbp + 8*(5 + rcx)]\n"
				"\tDATA rdx\n"
				"\tcmp rbx, rdx\n"
				"\tjge .retFalse\n"
				"\tinc rcx\n"
				"\tjmp .check_less_than_loop\n"

				".retFalse:\n\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n"

				".doneCheckLT:\n\n"
				"\tmov rax, SOB_TRUE\n"
				"\tjmp .done\n"

				".badArgs:\n\n"
				"\tmov rax, SOB_VOID\n"
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


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR FVAR+PVAR+BVAR EXPRESSION =========================================
;=========================================================================================================================================

(define handle_fvar
		(lambda (var depth const-table global-env)
			(string-append  "\tmov rax, [" (find-var-in-global-env var global-env) "]\n")))


(define handle_pvar
  (lambda (pe)
    (let ((min (caddr pe)))
      (string-append "\tmov rax, qword [rbp + (4+" (number->string min)   ")*8]\n"))))


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
;======================================================= FUNCTIONS FOR PLUS EXPRESSION ===================================================
;=========================================================================================================================================

(define handle_plus
		(lambda (global-env) 

			(string-append (applic-prolog "plus_code" "end_plus_code")

				"plus_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsAreNumbers:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .make_addition\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
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
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				"\tmov rdx, 0 ; rdx is the accumulator \n"

				".addition_loop:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .doneAddition\n\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
				"\tDATA rbx\n"
				"\tadd rdx, rbx\n"
				"\tinc rcx\n"
				"\tjmp .addition_loop\n"

				".doneAddition:\n\n"
				"\tmov rax, MAKE_LITERAL(T_INTEGER, rdx)\n"
				"\tjmp .done\n"

				".badArgs:\n\n"
				"\tmov rax, SOB_VOID\n"
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
;======================================================= FUNCTIONS FOR CHAR->INTEGER EXPRESSION ==========================================
;=========================================================================================================================================

(define handle_char->integer
		(lambda (global-env) 

			(string-append (applic-prolog "char_to_integer_code" "end_char_to_integer_code")

				"char_to_integer_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_CHAR\n"
				"\tjne .badInput ; not of type char - can't convert\n"
				"\txor rax, (T_INTEGER ^ T_CHAR)\n"
				"\tjmp .done\n\n"
				".badInput:\n\n"
				"\tmov rax, SOB_VOID\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "integer_to_char_code" "end_integer_to_char_code")

				"integer_to_char_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tjne .badInput ; not of type integer - can't convert\n"
				"\tmov rbx, rax\n"
				"\tDATA rbx\n"
				"\tcmp rbx, 0\n"
				"\tjl .badInput ; negative integer - can't convert to char because it doesn't have an ascii representation of type integer - can't convert\n"
				"\tmov rbx, rax\n"
				"\tDATA rbx\n"
				"\tcmp rbx, 256\n"
				"\tjge .badInput ; integer to large - can't convert to char because it doesn't have an ascii representation of type integer - can't convert\n"
				"\txor rax, (T_CHAR ^ T_INTEGER)\n"
				"\tjmp .done\n\n"
				".badInput:\n\n"
				"\tmov rax, SOB_VOID\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
;======================================================= FUNCTIONS FOR NUMERATOR EXPRESSION ==============================================
;=========================================================================================================================================

(define handle_numerator
		(lambda (global-env) 

			(string-append (applic-prolog "numerator_code" "end_numerator_code")

				"numerator_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notAFraction\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tjne .notAFraction\n"
				"\tNUMERATOR rax\n"
				"\tjmp .done\n\n"
				".notAFraction:\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "denominator_code" "end_denominator_code")

				"\ndenominator_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notAFraction\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tjne .notAFraction\n"
				"\tDENOMINATOR rax\n"
				"\tjmp .done\n\n"
				".notAFraction:\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "cons_code" "end_cons_code")

				"\ncons_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 2\n"
				"\tjne .badArgCount\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov rbx, rax ; will hold the car address \n"
				"\tmov rcx, qword [rbp + 4*8]\n"
				"\tmov qword [rbx], rcx\n"
				"\tpush rbx\n"
				"\tmov rdi, 8\n"
				"\tcall malloc\n"
				"\tmov rdx, rax ; will hold the cdr address \n"
				"\tmov rcx, qword [rbp + 5*8]\n"
				"\tmov qword [rdx], rcx\n"
				"\tpush rdx\n"
				"\tmov rdi, 8\n"
				"\tcall malloc ; rax will hold the address of the new pair\n"
				"\tpop rdx\n"
				"\tpop rbx\n"
				"\tMAKE_MALLOC_LITERAL_PAIR rax, rbx, rdx\n"
				"\tmov rax, [rax]\n"
				"\tjmp .done\n\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "car_code" "end_car_code")

				"\ncar_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notAPair\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_PAIR\n"
				"\tjne .notAPair\n"
				"\tCAR rax\n"
				"\tjmp .done\n\n"
				".notAPair:\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "cdr_code" "end_cdr_code")

				"\ncdr_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notAPair\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_PAIR\n"
				"\tjne .notAPair\n"
				"\tCDR rax\n"
				"\tjmp .done\n\n"
				".notAPair:\n"
				"\tmov rax, SOB_VOID\n"
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
;======================================================= FUNCTIONS FOR STRING? EXPRESSION ================================================
;=========================================================================================================================================

;(define handle_symbol? 
;		(lambda () 
;			(string-append "\nhandle_symbol?:\n"
;				"\tpush rbp\n"
;				"\tmov rbp, rsp\n"
;				"\tmov rax, qword [rbp + 8*4]\n"
;				"\tmov rbx, rax\n"
;				"\tTYPE rbx\n"
;				"\tcmp rbx, T_SYMBOL\n"
;				"\tje trueSymbol?\n"
;				"\tmov rax, SOB_FALSE\n"
;				"\tjmp doneSymbol?\n\n"
;				"trueSymbol?:\n"
;				"\tmov rax, SOB_TRUE\n\n"
;				"doneSymbol?:\n"
;				"\tmov rsp, rbp\n" "\tpop rbp\n"
;				"\tret\n\n")))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR STRING? EXPRESSION =========================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR ZERO? EXPRESSION ==================================================
;=========================================================================================================================================

(define handle_zero? 
		(lambda (global-env) 

			(string-append (applic-prolog "zero?_code" "end_zero?_code")


				"\nzero?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .notANumber\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .chechIfZero\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".chechIfZero:\n"
				"\tcmp rax, MAKE_LITERAL(T_INTEGER, 0)\n"
				"\tje .isZero\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".isZero:\n"
				"\tmov rax, SOB_TRUE\n"
				"\tjmp .done\n"
				".notANumber:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "not_code" "end_not_code")

				"\nnot_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_BOOL\n"
				"\tjne .retFalse\n"
				"\tmov rbx, rax\n"
				"\tcmp rbx, SOB_TRUE\n"
				"\tje .retFalse\n"
				"\tmov rax, SOB_TRUE\n"
				"\tjmp .done\n\n"
				".retFalse:\n"
				"\tmov rax, SOB_FALSE\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "vector?_code" "end_vector?_code")

				"\nvector?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_VECTOR\n"
				"\tje .trueVector?\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".trueVector?:\n"
				"\tmov rax, SOB_TRUE\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "string?_code" "end_string?_code")

				"\nstring?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_STRING\n"
				"\tje .trueString?\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".trueString?:\n"
				"\tmov rax, SOB_TRUE\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env) 

			(string-append (applic-prolog "char?_code" "end_char?_code")

				"\nchar?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_CHAR\n"
				"\tje .trueChar?\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".trueChar?:\n"
				"\tmov rax, SOB_TRUE\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
;======================================================= FUNCTIONS FOR PAIR? EXPRESSION ==================================================
;=========================================================================================================================================

(define handle_pair?
		(lambda (global-env)

			(string-append (applic-prolog "pair?_code" "end_pair?_code")

				"\npair?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_PAIR\n"
				"\tje .truePair?\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".truePair?:\n"
				"\tmov rax, SOB_TRUE\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env)

			(string-append (applic-prolog "boolean?_code" "end_boolean?_code")

				"\nboolean?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_BOOL\n"
				"\tje .trueBoolean?\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".trueBoolean?:\n"
				"\tmov rax, SOB_TRUE\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
;======================================================= FUNCTIONS FOR INTEGER? EXPRESSION ===============================================
;=========================================================================================================================================

(define handle_rational?
		(lambda (global-env)

			(string-append (applic-prolog "rational?_code" "end_rational?_code")
			
				"\nrational?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .trueRational?\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .trueRational?\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".trueRational?:\n"
				"\tmov rax, SOB_TRUE\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_rational?_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [rational?], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR INTEGER? EXPRESSION ========================================
;=========================================================================================================================================


;=========================================================================================================================================
;======================================================= FUNCTIONS FOR INTEGER? EXPRESSION ===============================================
;=========================================================================================================================================

(define handle_integer?
		(lambda (global-env)

			(string-append (applic-prolog "integer?_code" "end_integer?_code")

				"\ninteger?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .trueInteger?\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".trueInteger?:\n"
				"\tmov rax, SOB_TRUE\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env)

			(string-append (applic-prolog "null?_code" "end_null?_code")

				"\nnull?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_NIL\n"
				"\tje .trueNull?\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".trueNull?:\n"
				"\tmov rax, SOB_TRUE\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
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
		(lambda (global-env)

			(string-append (applic-prolog "number?_code" "end_number?_code")

				"\nnumber?_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				"\tmov rax, qword [rbp + 8*3]\n"
				"\tcmp rax, 1\n"
				"\tjne .badArgCount\n"
				"\tmov rax, qword [rbp + 8*4]\n"
				"\tmov rbx, rax\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_INTEGER\n"
				"\tje .trueNumber?\n"
				"\tcmp rbx, T_FRACTION\n"
				"\tje .trueNumber?\n"
				"\tmov rax, SOB_FALSE\n"
				"\tjmp .done\n\n"
				".trueNumber?:\n"
				"\tmov rax, SOB_TRUE\n\n"
				"\tjmp .done\n"
				".badArgCount:\n\n"
				"\tmov rax, SOB_VOID\n"
				".done:\n"
				"\tmov rsp, rbp\n" "\tpop rbp\n"
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
		(display "def-exp handle_define: ") (display def-exp) (newline)
		;(display "global-env handle_define: ") (display global-env) (newline) 
		(let ((free-var (find-var-in-global-env (cadar def-exp) global-env)))
			(display "free-var handle_define: ") (display free-var) (newline)
				(string-append 
					(code-gen (cadr def-exp) depth const-table global-env)
					"\tmov rbx, " free-var "\n" 
					"\tmov qword [rbx], rax\n"
					"\tmov rax, SOB_VOID\n\n"))))


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
						"\tcmp rax, SOB_FALSE\n" 
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
					"\tcmp rax, SOB_FALSE\n" 
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
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL (T_INTEGER, " (number->string (car const-table)) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((number? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":" "\n" "\tdq MAKE_LITERAL_FRACTION (" (find-const-in-pairs (numerator (car const-table)) table-in-pairs) ", " (find-const-in-pairs (denominator (car const-table)) table-in-pairs) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
			  ((boolean? (car const-table)) (if (equal? (car const-table) #t)
			  					  (string-append "sobTrue:" "\n" "\tdq SOB_TRUE\n\n" (create_const_for_assembly (cdr const-table) (cdr table-in-pairs) num))
			  					  (string-append "sobFalse:" "\n" "\tdq SOB_FALSE\n" (create_const_for_assembly (cdr const-table) (cdr table-in-pairs) num))))
			  ((char? (car const-table)) 
			  		(string-append (find-const-in-pairs (car const-table) table-in-pairs) ":\n" "\tdq MAKE_LITERAL(T_CHAR, " (number->string (char->integer (car const-table))) ")\n\n" (create_const_for_assembly (cdr const-table) table-in-pairs num)))
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
			  )))

;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR CONSTANT TABLE =============================================
;=========================================================================================================================================



;=========================================================================================================================================
;======================================================= FUNCTIONS FOR SYMBOL TABLE ======================================================
;=========================================================================================================================================

;(define make_symbol_table
;	(lambda (exp)
;		(cond ((and (list? exp) (null? exp)) exp)
;			  ((and (list? exp) (or (equal? (car exp) 'bvar) (equal? (car exp) 'pvar))) (list (cadr exp)))
;			  ((and (list? exp) (list? (car exp))) (append (make_symbol_table (car exp)) (make_symbol_table (cdr exp))))
;			  ((and (list? exp) (> (length exp) 0) (not (list? (car exp)))) (make_symbol_table (cdr exp))))))

;(define symbol_table
;	(lambda (input-file)
;		(remove-duplicates (make_symbol_table input-file))))



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
			;(display "create_global_env_for_assembly: ") (display (cadar global-env-table-as-pairs)) (newline) (newline)
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
	(lambda (fvar)
		(let ((name (symbol->string fvar)))
			(cond ((equal? ">" name) (list "greaterThan" fvar))
				  ((equal? "<" name) (list "lessThan" fvar))
				  ((equal? "=" name) (list "equal" fvar))
				  ((equal? "+" name) (list "plus" fvar))
				  ((equal? "/" name) (list "divide" fvar))
				  ((equal? "*" name) (list "multiply" fvar))
				  ((equal? "-" name) (list "substract" fvar))


				  ((equal? "char->integer" name) (list "charToInteger" fvar))
				  ((equal? "integer->char" name) (list "integerToChar" fvar))
				  ((equal? "make-string" name) (list "makeString" fvar))
				  ((equal? "make-vector" name) (list "makeVector" fvar))
				  ((equal? "set-car!" name) (list "setCar" fvar))
				  ((equal? "set-cdr!" name) (list "setCdr" fvar))

				  ((equal? "string-ref" name) (list "stringRef" fvar))
				  ((equal? "string-set!" name) (list "stringSet" fvar))
				  ((equal? "string-length" name) (list "stringLength" fvar))
				  ((equal? "make-string" name) (list "makeString" fvar))
				  ((equal? "make-vector" name) (list "makeVector" fvar))
				  ((equal? "string->symbol" name) (list "stringToSymbol" fvar))
				  ((equal? "symbol->string" name) (list "symbolToString" fvar))

				  ((equal? "vector-length" name) (list "vectorLength" fvar))
				  ((equal? "vector-ref" name) (list "vectorRef" fvar))
				  ((equal? "vector-set!" name) (list "vectorSet" fvar))


				  ;((member #\! (string->list name)) (list (string-replace name '! 'B) fvar))
				  ;((member #\< (string->list name)) (list (string-replace name '< 'L) fvar))
				  ;((member #\> (string->list name)) (list (string-replace name '> 'G) fvar))
				  ;((member #\= (string->list name)) (list (string-replace name '= 'E) fvar))
				  ;((member #\+ (string->list name)) (list (string-replace name '+ 'P) fvar))
				  ;((member #\/ (string->list name)) (list (string-replace name '/ 'D) fvar))
				  ;((member #\* (string->list name)) (list (string-replace name '* 'M) fvar))
				  ;((member #\- (string->list name)) (list (string-replace name '- 'S) fvar))
				  (else (list name fvar))))))

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