(load "qq.scm")

(define *reserved-words*
	'(and begin cond define do else if lambda let let* letrec or quasiquote unquote unquote-splicing quote set!))

(define lastElementInPair (lambda (lst) 
							(letrec ((loop (lambda (l)
												(if (not (pair? l)) 
												l
												(loop (cdr l))))))
							(loop lst))))

(define firstElementsInPair (lambda (lst) 
							(letrec ((loop (lambda (l)
												(if (not (pair? l)) 
												'()
												(append (list (car l)) (loop (cdr l)))))))
							(loop lst))))





;;(define remove_begin (lambda (exp)
;;						(cond ((or (not (list? exp)) (null? exp)) exp)
;;							  ((and (list? (car exp)) (equal? (caar exp) 'begin)) (map remove_begin (cdar exp)))
		





(define remove_begin (lambda (lst)
						(cond ((or (not (list? lst)) (null? lst)) lst) 
							  ((and (list? lst) (list? (car lst)) (null? (car lst))) (list '() (remove_begin (cdr lst))))
							  ;;((and (list? lst) (equal? (car lst) 'begin)) (remove_begin (cdr lst)))
							  ((and (list? lst) (list? (car lst))) (if (equal? (caar lst) 'begin)
							  										   (append (remove_begin (cdar lst)) (remove_begin (cdr lst)))
							  										   (append (list (car lst)) (remove_begin (cdr lst)))))
							  (else (append (list (car lst)) (remove_begin (cdr lst)))))))

;; '(begin a (begin 1 (begin b) 2) (if (begin 3 4) 5 6))




;; '(begin a (begin 5 (begin 1 2 3) 4) (begin b (f (begin c (begin d e (begin f g h) "Akuna Matata")))))

;; (seq ((var a) (const 5) (const 1) (const 2) (const 3) (const 4) (var b) (applic (var f) ((seq ((var c) (var d) (var e) (var f) (var g) (var h) (const "Akuna Matata")))))))




;; '(begin (lambda () (begin 'a 'b) 'c) (begin 'd))

;; '((lambda () (begin 'a 'b) 'c) (begin 'd))

;; '(lambda () (begin 'a 'b) 'c)

;; '(() (begin 'a 'b) 'c))

(define check_duplicates (lambda (lst) 
    							(cond ((<= (length lst) 1) #f)
    								  ((member (car lst) (cdr lst)) #t) 
    								  (else (check_duplicates (cdr lst))))))

(define check_length (lambda (exp)
							(cond ((null? exp) '())
								  ((< (length (car exp)) 2) (car exp))
								  (else (check_length (cdr exp))))))

(define make_set_for_letrec (lambda (params args)
								(if (and (null? params) (null? args))
								   '() 
									(append (list(list 'set! (car params) (car args))) (make_set_for_letrec (cdr params) (cdr args))))))


(define make_if
	(lambda (exp) 
            (cond ((= (length exp) 4) (let ((test (cadr exp))
					                        (dit (caddr exp))
					                        (dif (cadddr exp))) 
					                        `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
				  ((= (length exp) 3) (let ((test (cadr exp))
				      						(dit (caddr exp)))
						  					`(if3 ,(parse test) ,(parse dit) ,`(const ,(void)))))
				  (else (error 'parser (format "Unknown form: ~a" exp))))))


(define make_or
	(lambda (exp) (cond ((= (length exp) 1) `(const #f))
					 	((= (length exp) 2) (if (list? (cadr exp))
					 							(parse (cadr exp))
					 							`(const ,@(cdr exp))))
					 	(else `(or ,(map parse (cdr exp)))))))


(define make_lambda
	(lambda (exp) (let ((params (cadr exp))
					   (body (cddr exp)))
					   ;;(display "make_lambda_exp: ") (display exp) (newline)
					   ;;(display "make_lambda_params: ") (display params) (newline)
					   ;;(display "make_lambda_body: ") (display body) (newline)
                       (if (= (length body) 1)
                       		(cond ((list? params) (if (and (> (length params) 0) (list? (car (reverse params)))) 
                       								  (error 'parse (format "Invalid parameter list: ~a" params))
                       								  (list 'lambda-simple params (parse (car body)))))
                       		  	  ((pair? params) (list 'lambda-opt (firstElementsInPair params) (lastElementInPair params) (parse (car body))))
                       		      (else (list 'lambda-opt '() params (parse (car body)))))
	                       	(cond ((list? params) (if (and (> (length params) 0) (list? (car (reverse params)))) 
                       							  	  (error 'parse (format "Invalid parameter list: ~a" params))
                                 		  		  	  (list 'lambda-simple params (list 'seq (map parse (remove_begin body))))))
	                       		  ((pair? params) (list 'lambda-opt (firstElementsInPair params) (lastElementInPair params) (list 'seq (map parse (remove_begin body)))))
	                       		  (else (list 'lambda-opt '() params (list 'seq (map parse (remove_begin body))))))))))

;; '(begin (lambda () (begin 'a 'b) 'c) (begin 'd))


(define make_define
	(lambda (exp) (let ((params (cadr exp))
					    (body (cddr exp)))
						(if (= (length body) 1)
					    	(cond ((pair? params) (list 'define (list 'var (car params)) (make_lambda (append (list 'lambda) (list (cdr params)) body))))
					   		  	(else (list 'define (list 'var params) (parse (car body)))))
					    	(cond ((pair? params) (list 'define (list 'var (car params)) (make_lambda (append (list 'lambda) (list (cdr params)) body))))
					   		  	(else (list 'define (list 'var params) (list 'seq (map parse body)))))))))                              


(define make_set
	(lambda (exp)
		(append (list 'set) (list (list 'var (cadr exp))) (map parse (cddr exp)))))


(define make_applic
	(lambda (exp)
			(cond ((= (length exp) 1) (list 'applic (parse (car exp)) '()))
				  ((> (length exp) 1) (list 'applic (parse (car exp)) (map parse (cdr exp)))))))


(define make_sequence
	(lambda (exp) 
				 (cond ((= (length exp) 1) (list 'const (void)))
					   ((= (length exp) 2) (parse (cadr exp)))
					   (else (let ((s (remove_begin (cdr exp))))
					   			 ; (display "s: ") (newline) (display s) (newline) 
					  			  (list 'seq (map parse s)))))))

(define make_and 
	(lambda (exp) (if (<= (length exp) 1)  
					  (parse (car exp))
					  (list 'if3 (parse (car exp)) (make_and (cdr exp)) (list 'const #f)))))

(define make_let
	(lambda (exp) 
				(let ((body (cddr exp))
					   (params (map car (cadr exp)))
					   (args (map cadr (cadr exp))))
					   (parse (append (list (append (list 'lambda params) body)) args)))))

(define make_let*
	(lambda (exp) 
			(if (<= (length (cadr exp)) 1)
					(cons 'let (cdr exp))
					(list 'let (list (caadr exp)) (make_let* (append (list 'let*) (list (cdadr exp)) (cddr exp)))))))

(define make_applic_for_letrec
	(lambda (exp) (newline)
				  (cond ((= (length exp) 1) (list 'applic (parse (car exp)) '()))
						((> (length exp) 1) (list 'applic (parse (car exp)) (parse (cadr exp)))))))


(define make_letrec 
    (lambda (exp) 
           (let ((body (cddr exp))
				 (params (map car (cadr exp)))
				 (args (map cadr (cadr exp))))
				 (cond ((null? params) (parse (append (list (list 'lambda params (list (list 'lambda '() (append (list 'begin) body))))) (map (lambda (x) #f) params))))
				 		(else (parse (append (list (append (list 'lambda params) (make_set_for_letrec params args) (list (list (list 'lambda '() (append (list 'begin) body)))))) (map (lambda (x) #f) params))))))))



(define make_cond 
	(lambda (exp) 		
				(cond ((and (not (null? exp)) (eq? 'else (caar exp))) (if (<= (length (cdar exp)) 1)
												                          (parse (cadar exp))
												                          (list 'seq (map parse (cdar exp)))))	
					  ((= (length exp) 0) (list 'const (void)))
					  ((= (length (cdar exp)) 1) (list 'if3 (parse (caar exp)) (parse (cadar exp)) (make_cond (cdr exp))))
					  (else (list 'if3 (parse (caar exp)) (list 'seq (map parse (cdar exp))) (make_cond (cdr exp)))))))


(define parse
		(lambda (exp) 
					(if (list? exp) 
						  (cond ((null? exp) (error 'parser (format "Unknown form: ~a" exp)))
						  		((eq? (car exp) 'if) (make_if exp)) ;;; create if expression 
						  		((quote? exp) `(const ,@(cdr exp)))   ;;; create type 2 of constant 
						  		((eq? (car exp) 'or) (make_or exp))      ;;; create or
						  	    ((eq? (car exp) 'lambda) (cond ((= (length exp) 1) (error 'parser (format "Unknown form: ~a" exp))) ;;; create lambda
						  	    							   ((and (list? (cadr exp)) (check_duplicates (cadr exp))) (error 'parser (format "Invalid parameter list: ~a" (cadr exp))))
						  	    							   (else (make_lambda exp))))
						  	    ((eq? (car exp) 'define) (make_define exp)) ;;; create define
						  		((eq? (car exp) 'set!) (make_set exp))  ;;; create assignment
						  		((eq? (car exp) 'begin) (make_sequence exp)) ;;; create sequence
						  		((eq? (car exp) 'and) (cond ((= (length exp) 1) (list 'const #t))
						  									((= (length exp) 2) (parse (cadr exp)))
						  									((= (length exp) 3) (list 'if3 (parse (cadr exp)) (parse (caddr exp)) (list 'const #f)))
						  									(else (make_and (cdr exp))))) ;;; create and
						  		((eq? (car exp) 'let) (cond ((or (<= (length exp) 1) (null? (cddr exp))) (error 'parser (format "Unknown form: ~a" exp)))
															((and (list? (cadr exp)) (check_duplicates (map car (cadr exp)))) (error 'parser (format "Invalid parameter list: ~a" (map car (cadr exp)))))
						  									((make_let exp))))  ;;; create let
						  		((eq? (car exp) 'let*) (cond ((or (<= (length exp) 1) (null? (cddr exp))) (error 'parser (format "Unknown form: ~a" exp)))
															((and (list? (cadr exp)) (null? (cadr exp))) (make_applic (list (append (list 'lambda '()) (cddr exp)))))
						  									(else (parse (make_let* exp)))))  ;;; create let*
						  		((eq? (car exp) 'letrec) (cond ((or (= (length exp) 1) (not (list? (cadr exp)))) (error 'parser (format "Unknown form: ~a" exp)))
						  								  		(else (make_letrec exp)))) ;;; create letrec
						  		((eq? (car exp) 'cond) (cond ((= (length exp) 1) (error 'parser (format "Unknown form: ~a" exp)))
						  									 ((not (null? (check_length (cdr exp)))) (error 'parser (format "This is not a valid cond-rib: : ~a" (check_length (cdr exp)))))
						  									 (else (make_cond (cdr exp)))))
						  		((eq? (car exp) 'quasiquote) (cond ((or (= (length exp) 1) (> (length exp) 2)) (error 'parser (format "Unknown form: ~a" exp)))
						  										   (else (parse (expand-qq (cadr exp))))))
						  		(else (make_applic exp)))  ;;; create application
						  (cond ((and (symbol? exp) (not (memq exp *reserved-words*))) `(var ,exp))  ;;; create variable   
								((or (null? exp) (vector? exp) (boolean? exp) (char? exp) (number? exp) (string? exp)) `(const ,exp))  ;;; create type 1 of constant 
				  		    	))))   


