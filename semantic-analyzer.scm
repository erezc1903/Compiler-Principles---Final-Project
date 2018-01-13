


(define find-location
		(lambda (lst item loc)
			(cond ((equal? (car lst) item) loc)
				  (else (find-location (cdr lst) item (+ loc 1))))))



(define check-if-in-params
		(lambda (params body)
			;(newline) (display "params in check-if-in-params: ") (display params) (newline)
			;(display "body in check-if-in-params: ") (display body) (newline)
			(map (lambda (b) 
					;(display "b in check-if-in-params: ") (display b) (newline)
					(cond ((or (not (list? b)) (null? b)) b)
						  ((and (= (length b) 2) (equal? (car b) 'var) (member (cadr b) params)) (list 'pvar (cadr b) (find-location params (cadr b) 0)))
						  ((and (= (length b) 2) (equal? (car b) 'var)) b)
						  ((or (equal? (car b) 'lambda-simple) (equal? (car b) 'lambda-opt)) (pe->lex-pe-pvar b))
						  ((and (> (length b) 1) (not (list? (car b))) (cons (car b) (check-if-in-params params (cdr b)))))
						  (else (check-if-in-params params b)))) body)))


'(seq (set (var h) (lambda-simple (j) (applic (bvar x 1 0) ((var j) (bvar y 3 0))))) (var h))




(define member-in-some-list
		(lambda (param lst)
			(cond ((null? lst) lst)
				  ((member param (car lst)) (car lst))
					(else (member-in-some-list param (cdr lst))))))

(define find-major
		(lambda (lst item loc)
			(cond ((null? (car lst)) (find-major (cdr lst) item (+ loc 1)))
				  ((member item (car lst)) loc)
				  (else (find-major (cdr lst) item (+ loc 1))))))

(define find-minor
		(lambda (lst item)
			(cond ((member item (car lst)) (car lst))
				  (else (find-minor (cdr lst) item)))))

(define locations
		(lambda (lst item)
				(let ((major (find-major (cdr lst) item 0))
					  (minor (find-minor lst item)))
					  (list major (find-location minor item 0)))))

(define check-if-bound
		(lambda (current-params body aggregated-params)
			;(newline) (display "current-params in check-if-bound: ") (display current-params) (newline)
			;(display "body in check-if-bound: ") (display body) (newline)
			;(display "aggregated-params in check-if-bound: ") (display aggregated-params) (newline) (newline)
			(map (lambda (b)
				;(display "b in check-if-bound: ") (display b) (newline)
				(cond ((or (not (list? b)) (null? b)) b)
					  ((and (= (length b) 2) (equal? (car b) 'var) (member (cadr b) current-params)) b)
					  ((and (= (length b) 2) (equal? (car b) 'var) (not (null? (member-in-some-list (cadr b) aggregated-params)))) (append (list 'bvar (cadr b)) (locations aggregated-params (cadr b))))
					  ((or (equal? (car b) 'lambda-simple) (equal? (car b) 'lambda-opt)) (pe->lex-pe-bvar b aggregated-params))
					  (else (check-if-bound current-params b aggregated-params)))) body)))


'((lambda-simple (x) (applic (var x) ((lambda-simple (y) (applic (var x) ((lambda-simple (y) (applic (var x) ((lambda-simple (y) (var y))))))))))) ())

(define check-if-free
		(lambda (exp lst)
				(map (lambda (b) 
					(cond ((or (not (list? b)) (null? b)) b)
						  ((and (= (length b) 2) (equal? (car b) 'var) (not (member (cadr b) lst))) (list 'fvar (cadr b)))
						  ((and (= (length b) 2) (equal? (car b) 'var)) b)
						  ((or (equal? (car b) 'lambda-simple) (equal? (car b) 'lambda-opt)) (pe->lex-pe-fvar b lst))
						  (else (check-if-free b lst)))) exp)))


(define pe->lex-pe-pvar
	(lambda (exp)
			;(newline)(display "exp in pe->lex-pe-pvar: ") (display exp) (newline)
			(cond ((and (> (length exp) 1) (equal? (car exp) 'seq)) (list (car exp) (map pe->lex-pe (cadr exp))))
				  ((and (> (length exp) 1) (equal? (car exp) 'or)) (list (car exp) (map pe->lex-pe (cadr exp))))
				  ((and (list? exp) (= (length exp) 2) (not (list? (car exp))) (not (list? (cadr exp)))) exp)
				  ((and (list? exp) (= (length exp) 2) (not (list? (car exp))) (list? (cadr exp))) (list (car exp) (pe->lex-pe-pvar (cdr exp))))
				  ((and (not (null? exp)) (equal? (car exp) 'lambda-simple)) ;(newline) (display "(car (check-if-in-params (cadr exp) (cddr exp))) in pe->lex-pe-pvar:") (display (car (check-if-in-params (cadr exp) (cddr exp)))) (newline) 
				  															 (list (car exp) (cadr exp) (car (check-if-in-params (cadr exp) (cddr exp)))))
				  ((and (not (null? exp)) (equal? (car exp) 'lambda-opt)) (list (car exp) (cadr exp) (caddr exp) (car (check-if-in-params (append (cadr exp) (list (caddr exp))) (cdddr exp)))))
				  ((and (> (length exp) 1) (equal? (car exp) 'applic) (null? (car (reverse exp)))) (list (car exp) (pe->lex-pe-pvar (cadr exp)) '()))
				  ((and (> (length exp) 1) (equal? (car exp) 'applic)) (list (car exp) (pe->lex-pe (cadr exp)) (map pe->lex-pe (caddr exp))))
				  ((and (> (length exp) 1) (equal? (car exp) 'if3)) (list (car exp) (pe->lex-pe (cadr exp)) (pe->lex-pe (caddr exp)) (pe->lex-pe (cadddr exp))))
				  ((and (> (length exp) 1) (equal? (car exp) 'seq)) (list (car exp) (map pe->lex-pe-pvar (cadr exp))))
				  ((and (> (length exp) 1) (not (list? (car exp))) (cons (car exp) (pe->lex-pe-pvar (cdr exp)))))
				  ((and (> (length exp) 1) (list? (car exp)) (list (pe->lex-pe-pvar (car exp)) (list (pe->lex-pe-pvar (cdr exp))))))
				  ((and (= (length exp) 1) (list? (car exp))) (pe->lex-pe-pvar (car exp)))
				  (else exp))))


(define pe->lex-pe-fvar
		(lambda (exp lst)
			;(newline)(display "exp in pe->lex-pe-fvar: ") (display exp) (newline)
			;(display "lst in a pe->lex-pe-fvar: ") (display lst) (newline)
			(cond ((and (> (length exp) 1) (equal? (car exp) 'seq)) (list (car exp) (map pe->lex-pe (cadr exp))))
				  ((and (> (length exp) 1) (equal? (car exp) 'or)) (list (car exp) (map pe->lex-pe (cadr exp))))
				  ((and (list? exp) (= (length exp) 2) (not (list? (car exp))) (list? (cadr exp))) (list (car exp) (pe->lex-pe-fvar (cdr exp) lst)))
				  ((and (not (null? exp)) (equal? (car exp) 'lambda-simple)) (list (car exp) (cadr exp) (car (check-if-free (cddr exp) (append lst (cadr exp))))))
				  ((and (not (null? exp)) (equal? (car exp) 'lambda-opt)) (list (car exp) (cadr exp) (caddr exp) (car (check-if-free (cdddr exp) (append lst (append (cadr exp) (list (caddr exp))))))))
				  ((and (= (length exp) 2) (equal? (car exp) 'var) (not (member (cadr exp) lst))) (list 'fvar (cadr exp)))
				  ((and (> (length exp) 1) (equal? (car exp) 'applic) (null? (car (reverse exp)))) (list (car exp) (pe->lex-pe-fvar (cadr exp) lst) '()))
				  ((and (> (length exp) 1) (equal? (car exp) 'applic)) (list (car exp) (pe->lex-pe (cadr exp)) (map pe->lex-pe (caddr exp))))
				  ((and (> (length exp) 1) (equal? (car exp) 'if3)) (list (car exp) (pe->lex-pe (cadr exp)) (pe->lex-pe (caddr exp)) (pe->lex-pe (cadddr exp))))
				  ((and (> (length exp) 1) (equal? (car exp) 'seq)) (list (car exp) (map pe->lex-pe-fvar (cadr exp) lst)))
				  ((and (> (length exp) 1) (not (list? (car exp))) (cons (car exp) (pe->lex-pe-fvar (cdr exp) lst))))
				  ((and (> (length exp) 1) (list? (car exp)) (list (pe->lex-pe-fvar (car exp) lst) (list(pe->lex-pe-fvar (cdr exp) lst)))))
				  ((and (= (length exp) 1) (list? (car exp))) (pe->lex-pe-fvar (car exp) lst))
				  (else exp))))


(define pe->lex-pe-bvar
	(lambda (exp lst)
			;(newline)(display "exp in pe->lex-pe-bvar: ") (display exp) (newline)
			;(display "lst in a pe->lex-pe-bvar: ") (display lst) (newline)
			(cond ((or (not (list? exp)) (null? exp)) exp)
				  ((and (list? exp) (= (length exp) 1) (list? (car exp)) (null? (car exp))) (car exp))
				  ((and (list? exp) (= (length exp) 2) (not (list? (car exp))) (not (list? (cadr exp)))) exp)
				  ((and (> (length exp) 1) (equal? (car exp) 'seq)) (list (car exp) (map pe->lex-pe (cadr exp))))
				  ((and (> (length exp) 1) (equal? (car exp) 'or)) (list (car exp) (map pe->lex-pe (cadr exp))))
				  ((and (list? exp) (= (length exp) 2) (not (list? (car exp))) (list? (cadr exp))) (list (car exp) (pe->lex-pe-bvar (cdr exp) lst)))
				  ((and (not (null? exp)) (equal? (car exp) 'lambda-simple)) (list (car exp) (cadr exp) (car (check-if-bound (cadr exp) (cddr exp) (append (list (cadr exp)) lst)))))
				  ((and (not (null? exp)) (equal? (car exp) 'lambda-opt)) (list (car exp) (cadr exp) (caddr exp) (car (check-if-bound (append (cadr exp) (list (caddr exp))) (cdddr exp) (append (list (append (cadr exp) (list (caddr exp)))) lst)))))
				  ((and (> (length exp) 1) (equal? (car exp) 'applic) (null? (car (reverse exp)))) (list (car exp) (pe->lex-pe-bvar (cadr exp) lst) '()))
				  ((and (> (length exp) 1) (equal? (car exp) 'applic)) (list (car exp) (pe->lex-pe (cadr exp)) (map pe->lex-pe (caddr exp))))
				  ((and (> (length exp) 1) (equal? (car exp) 'if3)) (list (car exp) (pe->lex-pe-bvar (cadr exp) lst) (pe->lex-pe (caddr exp)) (pe->lex-pe (cadddr exp))))
				  ((and (> (length exp) 1) (not (list? (car exp))) (cons (car exp) (pe->lex-pe-bvar (cdr exp) lst))))
				  ((and (> (length exp) 1) (list? (car exp)) (list (pe->lex-pe-bvar (car exp) lst) (list (pe->lex-pe-bvar (cdr exp) lst)))))
				  ((and (= (length exp) 1) (list? (car exp))) (pe->lex-pe-bvar (car exp) lst))
				  (else exp))))
'((if3 (if3 (applic (fvar =) ((pvar num 1) (const 0))) (applic (fvar null?) ((pvar exp 0))) (const #f)) (const #t) (if3 (or ((applic (fvar <) ((pvar num 1) (const 0))) (applic (fvar null?) ((pvar exp 0))))) (const #f) (if3 (applic (fvar char=?) ((applic (fvar car) ((pvar exp 0))) (const ())) (applic (box-get (bvar are-parentheses-balanced-list? 0 0)) ((applic (fvar cdr) ((pvar exp 0))) (applic (fvar +) ((pvar num 1) (const 1))))) (if3 (applic (fvar char=?) ((applic (fvar car) ((pvar exp 0))) (const )))) (applic (box-get (bvar are-parentheses-balanced-list? 0 0)) ((applic (fvar cdr) ((pvar exp 0))) (applic (fvar -) ((pvar num 1) (const 1))))) (applic (box-get (bvar are-parentheses-balanced-list? 0 0)) ((applic (fvar cdr) ((pvar exp 0))) (pvar num 1))))))))


(define handle-sequence-in-lambda
		(lambda (exp)
			(newline)(display "exp in handle-sequence-in-lambda: ") (display exp) (newline)
			(cond ((and (= (length exp) 2) (not (list? (car exp))) (not (list? (cadr exp)))) (list exp))
				  ((and (list? exp) (= (length exp) 2) (equal? (car exp) 'const)) (list exp))
				  ((or (not (list? exp)) (null? exp)) exp)
				  ((and (= (length exp) 1) (list? (car exp)) (not (equal? (caar exp) 'applic)) (not (equal? (caar exp) 'if3))) (handle-sequence-in-lambda (car exp)))
				  ((and (list? exp) (list? (car exp)) (equal? (caar exp) 'if3) (equal? (caadr (car exp)) 'if3)) (list (handle-if-in-test (car exp))))
				  ((and (list? exp) (list? (car exp)) (equal? (caar exp) 'if3) (null? (cdr exp))) 
				  		(list (let ((test (cadar exp))
  			  						(dit (caddar exp))
  			  						(dif (cadddr (car exp))))
  			  						(list (caar exp) (annotate-tc test) (cond ((equal? (car dit) 'if3) (car (handle-sequence-in-lambda (list dit))))
  			  																  ((equal? (car dit) 'applic) (list 'tc-applic (annotate-tc (cadr dit)) (map annotate-tc (caddr dit))))
  			  									   							  ((or (equal? (car dit) 'seq) (equal? (car dit) 'or)) (list (car dit) (handle-sequence-in-lambda (cdr dit))))
  			  									   							  (else (annotate-tc dit)))
											  			  				(cond ((equal? (car dif) 'if3) (car (handle-sequence-in-lambda (list dif))))
											  			  					  ((equal? (car dif) 'applic) (list 'tc-applic (annotate-tc (cadr dif)) (map annotate-tc (caddr dif))))
											  			  					  ((or (equal? (car dif) 'seq) (equal? (car dif) 'or)) (list (car dif) (handle-sequence-in-lambda (cdr dif))))
											  			  					  (else (annotate-tc dif)))))))
				  ((and (list? exp) (list? (car exp)) (equal? (caar exp) 'if3) (not (null? (cdr exp)))) 
				  		(cons (let ((test (cadar exp))
  			  						(dit (caddar exp))
  			  						(dif (cadddr (car exp))))
			  			  			(list (caar exp) (annotate-tc test) (cond ((equal? (car dit) 'if3) (car (handle-sequence-in-lambda (list dit))))
			  			  													  ((equal? (car dit) 'applic) (list 'applic (annotate-tc (cadr dit)) (map annotate-tc (caddr dit))))
			  			  									   				  ((or (equal? (car dit) 'seq) (equal? (car dit) 'or)) (list (car dit) (handle-sequence-in-lambda (cdr dit))))
			  			  									   				  (else (annotate-tc dit)))
														  			    (cond ((equal? (car dif) 'if3) (car (handle-sequence-in-lambda (list dif))))
														  			    	  ((equal? (car dif) 'applic) (list 'applic (annotate-tc (cadr dif)) (map annotate-tc (caddr dif))))
														  			  		  ((or (equal? (car dif) 'seq) (equal? (car dif) 'or)) (list (car dif) (handle-sequence-in-lambda (cdr dif))))
														  			  		  (else (annotate-tc dif)))))
									(handle-sequence-in-lambda (cdr exp))))
				  ((null? (cdr exp)) (if (and (list? (car exp)) (equal? (caar exp) 'applic))
				  						 (list (list 'tc-applic (annotate-tc (cadar exp)) (map annotate-tc (caddar exp))))
				  						 (annotate-tc (car exp))))
				  ((and (= (length exp) 2) (or (equal? (car exp) 'box) (equal? (car exp) 'box-get))) (list exp))
				  ((and (> (length exp) 1) (or (equal? (car exp) 'lambda-simple) (equal? (car exp) 'lambda-opt))) (list (annotate-tc exp)))
				  ((and (> (length exp) 1) (or (equal? (car exp) 'pvar) (equal? (car exp) 'bvar)))  (list exp))
				  ((and (> (length exp) 1) (or (equal? (car exp) 'box-set) (equal? (car exp) 'set))) (list (list (car exp) (cadr exp) (annotate-tc (cddr exp)))))
				  (else (cons (annotate-tc (car exp)) (handle-sequence-in-lambda (cdr exp)))))))

(define create-tc-app
		(lambda (exp)
			(newline)(display "exp in create-tc-app: ") (display exp) (newline)
			(cond ((and (list? (car exp)) (or (equal? (caar exp) 'lambda-simple) (equal? (caar exp) 'lambda-opt))) (annotate-tc exp))
				  ((and (= (length exp) 1) (list? (car exp)) (equal? (caar exp) 'applic)) (list 'tc-applic (annotate-tc (cadar exp)) (map annotate-tc (caddar exp))))
				  ((and (= (length exp) 1) (list? (car exp)) (or (equal? (caar exp) 'seq) (equal? (caar exp) 'or))) (list (caar exp) (handle-sequence-in-lambda (cdar exp))))
				  ((and (= (length exp) 1) (list? (car exp)) (equal? (caar exp) 'if3)) 
				  				(let ((test (cadr (car exp)))
  			  						  (dit (caddr (car exp)))
  			  						  (dif (cadddr (car exp))))
  			  						  (list (caar exp) (annotate-tc test) (cond ((equal? (car dit) 'if3) (car (handle-sequence-in-lambda (list dit))))
  			  						  											((equal? (car dit) 'applic) (list 'tc-applic (annotate-tc (cadr dit)) (map annotate-tc (caddr dit))))
  			  									   							    ((or (equal? (car dit) 'seq) (equal? (car dit) 'or)) (list (car dit) (handle-sequence-in-lambda (cdr dit))))
  			  									   							    (else (annotate-tc dit)))
  			  									   						  (cond ((equal? (car dif) 'if3) (car (handle-sequence-in-lambda (list dif))))
  			  									   						  		((equal? (car dif) 'applic) (list 'tc-applic (annotate-tc (cadr dif)) (map annotate-tc (caddr dif))))
  			  									   							  	((or (equal? (car dif) 'seq) (equal? (car dif) 'or)) (list (car dif) (handle-sequence-in-lambda (cdr dif))))
  			  									   							  	(else (annotate-tc dif))))))
				  (else (annotate-tc exp)))))



(define remove-applic-lambda-nil
		(lambda (exp)
			;(newline)(display "exp in remove-applic-lambda-nil: ") (display exp) (newline)
			(cond ((and (list? exp) (= (length exp) 3) (equal? (car exp) 'applic) (equal? (caadr exp) 'lambda-simple) (null? (cadadr exp)) (null? (caddr exp))) 
				  		(remove-applic-lambda-nil (car (cddadr exp))))
				  ((and (list? exp) (= (length exp) 3) (equal? (car exp) 'applic) (equal? (caadr exp) 'lambda-opt) (null? (cadadr exp)) (list? (car (cddadr exp))) (null? (caddr exp))) 
				  		(remove-applic-lambda-nil (car (cddadr exp))))
				  ((and (list? exp) (>= (length exp) 1)) (cons (remove-applic-lambda-nil (car exp)) (remove-applic-lambda-nil (cdr exp))))
				  (else exp))))


(define check-if-bound-for-box-set
		(lambda (nested param body)
				;(newline)(display "body in check-if-bound-for-box-set: ") (display body) (newline)
				;(display "param in a check-if-bound-for-box-set: ") (display param) (newline)
				;(display "nested in a check-if-bound-for-box-set: ") (display nested) (newline)
				(cond ((null? body) #f)
					  ((and (list? (car body)) (null? (car body))) (check-if-bound-for-box-set nested param (cdr body)))
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-simple) (member param (cadar body))) #f)
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-opt) (member param (cons (caddar body) (cadar body)))) #f)
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-simple) (not (member param (cadar body)))) (or (check-if-bound-for-box-set #t param (cddar body)) (check-if-bound-for-box-set nested param (cdr body))))
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-opt) (not (member param (cons (caddar body) (cadar body))))) (or (check-if-bound-for-box-set #t param (cdddar body)) (check-if-bound-for-box-set nested param (cdr body))))
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'var) (equal? (cadar body) param) (equal? nested #f)) (check-if-bound-for-box-set nested param (cdr body)))
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'var) (equal? (cadar body) param) (equal? nested #t)) #t)
					  ((and (list? (car body)) (not (null? (car body)))) (or (check-if-bound-for-box-set nested param (car body)) (check-if-bound-for-box-set nested param (cdr body))))
					  ((not (list? (car body))) (check-if-bound-for-box-set nested param (cdr body)))
					  ((and (= (length body) 1) (list? (car body)) (check-if-bound-for-box-set nested param (car body)))))))


(define check-if-get-for-box-set
		(lambda (param body)
				;(display "body in check-if-get-for-box-set: ") (display body) (newline)
				;(display "param in a check-if-get-for-box-set: ") (display param) (newline)(newline)
				(cond ((null? body) #f)
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-simple) (member param (cadar body))) #f)
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-opt) (member param (cons (caddar body) (cadar body)))) #f)
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-simple) (not (member param (cadar body)))) (or (check-if-get-for-box-set param (cddar body)) (check-if-get-for-box-set param (cdr body))))
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-opt) (not (member param (cons (caddar body) (cadar body))))) (or (check-if-get-for-box-set param (cdddar body)) (check-if-get-for-box-set param (cdr body))))
					  ((and (equal? (car body) 'set) (equal? (cadadr body) param)) (check-if-get-for-box-set param (caddr body)))
					  ((and (equal? (car body) 'set) (equal? (cadadr body) param)) #f)
					  ((and (list?  body) (equal? (car body) 'var) (equal? (cadr body) param)) #t)
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'var) (equal? (cadar body) param)) #t)
					  ((list? (car body)) (or (check-if-get-for-box-set param (car body)) (check-if-get-for-box-set param (cdr body))))
					  ((not (list? (car body))) (check-if-get-for-box-set param (cdr body)))
					  ((and (= (length body) 1) (list? (car body)) (check-if-get-for-box-set param (car body)))))))
			

(define check-if-set
		(lambda (param body)
				(cond ((null? body) #f)
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-simple) (member param (cadar body))) #f)
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-opt) (member param (cons (caddar body) (cadar body)))) #f)
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-simple) (not (member param (cadar body)))) (or (check-if-set param (cddar body)) (check-if-set param (cdr body))))
					  ((and (list? (car body)) (not (null? (car body))) (equal? (caar body) 'lambda-opt) (not (member param (cons (caddar body) (cadar body))))) (or (check-if-set param (cdddar body)) (check-if-set param (cdr body))))
					  ((and (equal? (car body) 'set) (equal? (cadadr body) param)) #t)
					  ((and (list? (car body)) (not (null? (car body))) (not (or (equal? (caar body) 'lambda-simple) (equal? (caar body) 'lambda-opt)))) (or (check-if-set param (car body)) (check-if-set param (cdr body))))
					  ((not (list? (car body))) (check-if-set param (cdr body)))
					  ((and (= (length body) 1) (list? (car body))) (check-if-set param (car body))))))

(define change-set-to-box-set
		(lambda (param body)
				;(display "body in change-set-to-box-set: ") (display body) (newline)
				;(display "param in a change-set-to-box-set: ") (display param) (newline)(newline)
				(cond ((or (not (list? body)) (null? body)) body)
					  ((and (list? body) (= (length body) 2) (equal? (car body) 'const)) body)
					  ((and (list? body) (list? (car body)) (null? (car body))) (list (car body) (change-set-to-box-set param (cdr body))))
					  ((and (list? (car body)) (equal? (caar body) 'lambda-simple) (member param (cadar body))) (list (car body)))
					  ((and (list? (car body)) (equal? (caar body) 'lambda-opt) (member param (cons (caddar body) (cadar body)))) (list (car body)))
					  ((and (list? (car body)) (equal? (caar body) 'lambda-simple) (not (member param (cadar body)))) (cons (list (caar body) (cadar body) (change-set-to-box-set param (caddar body))) (change-set-to-box-set param (cdr body))))
					  ((and (list? (car body)) (equal? (caar body) 'lambda-opt) (not (member param (cons (caddar body) (cadar body))))) (cons (list (caar body) (cadar body) (caddar body) (change-set-to-box-set param (car (cdddar body)))) (change-set-to-box-set param (cdr body))))
					  ((and (list? body) (not (null? body)) (equal? (car body) 'lambda-simple) (member param (cadr body))) (list body))
					  ((and (list? body) (not (null? body)) (equal? (car body) 'lambda-opt) (member param (cons (caddr body) (cadr body)))) (list body))
					  ((and (list? body) (not (null? body)) (equal? (car body) 'lambda-simple) (not (member param (cadr body)))) (list (car body) (cadr body) (change-set-to-box-set param (cddr body))))
					  ((and (list? body) (not (null? body)) (equal? (car body) 'lambda-opt) (not (member param (cons (caddr body) (cadr body))))) (list (car body) (cadr body) (caddr body) (change-set-to-box-set param (cdddr body))))
					  ((and (list? body) (not (null? body)) (equal? (car body) 'var) (equal? (cadr body) param)) (list 'box-get body))
					  ((and (equal? (car body) 'applic)) (list (car body) (change-set-to-box-set param (cadr body)) (change-set-to-box-set param (caddr body))))
					  ((and (equal? (car body) 'set) (equal? (cadadr body) param)) (list 'box-set (cadr body) (change-set-to-box-set param (cddr body))))
					  ((list? (car body)) (cons (change-set-to-box-set param (car body)) (change-set-to-box-set param (cdr body))))
					  ((not (list? (car body))) (cons (car body) (change-set-to-box-set param (cdr body))))
					  ((and (= (length body) 1) (list? (car body))) (change-set-to-box-set param (car body))))))

'(lambda-simple (res-lst) (applic (var append) ((applic (var list) ((applic (var append) ((applic (var list) ((var a))) (applic (var filter) ((lambda-simple (res-lst-element) (applic (var >) ((applic (var car) ((var res-lst-element))) (applic (var *) ((var a) (var a)))))) (var res-lst))))))) (var res-lst))))

(define add-set-and-seq-to-lambda 
			(lambda (body params)
				;(newline)(display "body in add-set-and-seq-to-lambda: ") (display body) (newline)
				;(display "params in add-set-and-seq-to-lambda: ") (display params) (newline)(newline)
				(let ((set-params (map (lambda (p) (list 'set (list 'var p) (list 'box (list 'var p)))) params)))
					;(newline)(display "set-params in add-set-and-seq-to-lambda: ") (display set-params) (newline)
					;(newline)(display "cadar body in add-set-and-seq-to-lambda: ") (display (cadar body)) (newline)(newline)
					  (if (equal? (caar body) 'seq)
					  	  (list 'seq (append set-params (cadar body)))
					  	  (list 'seq (append set-params body))))))


(define send-to-change-set-to-box-set
		(lambda (body params)
			;(newline)(display "body in send-to-change-set-to-box-set: ") (display body) (newline)
			;(display "params in send-to-change-set-to-box-set: ") (display params) (newline)(newline)
				(if (null? params)
					body
					(send-to-change-set-to-box-set (change-set-to-box-set (car params) body) (cdr params)))))

(define check-box-terms-in-lambda
		(lambda (params body)
			;(display "params in a check-box-terms-in-lambda: ") (display params) (newline)
			;(display "body in a check-box-terms-in-lambda: ") (display body) (newline)(newline)
				(let* ((bound-params (filter (lambda (p) (check-if-bound-for-box-set #f p body)) params))
						(set-bound-params (filter (lambda (p) (check-if-set p body)) bound-params))
						(legal-box-exp (filter (lambda (p) (check-if-get-for-box-set p body)) set-bound-params)))
						;(newline) (display "bound-params in check-box-terms-in-lambda: ") (display bound-params) (newline)
						;(display "set-bound-params in check-box-terms-in-lambda: ") (display set-bound-params) (newline)
						;(display "legal-box-exp in check-box-terms-in-lambda: ") (display legal-box-exp) (newline)(newline)
						(if (null? legal-box-exp)
							(car body)
							(add-set-and-seq-to-lambda (send-to-change-set-to-box-set body legal-box-exp) legal-box-exp)))))
(define box-set
  		(lambda (exp)
  			;(display "exp in box-set: ") (display exp) (newline) (newline)
  			(cond ((or (not (list? exp)) (null? exp)) exp)
  				  ((and (list? exp) (= (length exp) 2) (equal? (car exp) 'const)) exp)
  				  ((and (= (length exp) 2) (not (list? (car exp))) (not (list? (cadr exp)))) exp)
  				  ((and (list? exp) (not (null? exp)) (equal? (car exp) 'lambda-simple)) (list (car exp) (cadr exp) (box-set (check-box-terms-in-lambda (cadr exp) (cddr exp)))))
  				  ((and (list? exp) (not (null? exp)) (equal? (car exp) 'lambda-opt)) (list (car exp) (cadr exp) (caddr exp) (box-set (check-box-terms-in-lambda (append (cadr exp) (list (caddr exp))) (cdddr exp)))))
  				  ((and (equal? (car exp) 'if3)) (let ((test (cadr exp))
  			  									   (dit (caddr exp))
  			  									   (dif (cadddr exp)))
  			  									   (list (car exp) (box-set test) (box-set dit) (box-set dif))))
  				  ((and (= (length exp) 2) (or (equal? (car exp) 'box) (equal? (car exp) 'box-get)))  exp)
  				  ((and (> (length exp) 1) (equal? (car exp) 'applic)) (list (car exp) (box-set (cadr exp)) (map box-set (caddr exp))))
  				  ((and (> (length exp) 1) (equal? (car exp) 'seq)) (list (car exp) (map box-set (cadr exp))))
  				  ((and (> (length exp) 1) (equal? (car exp) 'or)) (list (car exp) (map box-set (cadr exp))))
  				  ((and (> (length exp) 1) (not (list? (car exp))) (cons (car exp) (box-set (cdr exp)))))
			  	  ((and (> (length exp) 1) (list? (car exp)) (list (box-set (car exp)) (box-set (cdr exp)))))
			  	  ((and (= (length exp) 1) (list? (car exp))) (box-set (car exp)))
			  	  (else exp))))

(define create-if-in-test 
		(lambda (exp)
			(let ((inside-test (cadr exp))
  			  	  (inside-dit (caddr exp))
  			  	  (inside-dif (cadddr exp)))
  			  	  (list (car exp) (annotate-tc inside-test) (cond ((equal? (car inside-dit) 'applic) (list 'applic (annotate-tc (cadr inside-dit)) (map annotate-tc (caddr inside-dit))))
  			  									   		      	  ((or (equal? (car inside-dit) 'seq) (equal? (car inside-dit) 'or)) (list (car inside-dit) (handle-sequence-in-lambda (cdr inside-dit))))
  			  									   		      	  (else (annotate-tc inside-dit)))
  			  									   	 		(cond ((equal? (car inside-dif) 'applic) (list 'applic (annotate-tc (cadr inside-dif)) (map annotate-tc (caddr inside-dif))))
  			  									   		   		  ((or (equal? (car inside-dif) 'seq) (equal? (car inside-dif) 'or)) (list (car inside-dif) (handle-sequence-in-lambda (cdr inside-dif))))
  			  									   		   		  (else (annotate-tc inside-dif)))))))
(define handle-if-in-test
		(lambda (exp)
			;(newline)(display "exp in handle-if-in-test: ") (display exp) (newline)
			(let ((outside-test (cadr exp))
  			  	   (outside-dit (caddr exp))
  			  	   (outside-dif (cadddr exp)))
  			  	   (list (car exp) (create-if-in-test outside-test) (cond ((equal? (car outside-dit) 'if3) (handle-sequence-in-lambda  outside-dit))
  			  	   														  ((equal? (car outside-dit) 'applic) (list 'tc-applic (annotate-tc (cadr outside-dit)) (map annotate-tc (caddr outside-dit))))
  			  									   		      			  ((or (equal? (car outside-dit) 'seq) (equal? (car outside-dit) 'or)) (list (car outside-dit) (handle-sequence-in-lambda (cdr outside-dit))))
  			  									   		      			  (else (annotate-tc outside-dit)))
  			  									   	 		  		(cond ((equal? (car outside-dif) 'if3) (handle-sequence-in-lambda outside-dif))
  			  									   	 		  			  ((equal? (car outside-dif) 'applic) (list 'tc-applic (annotate-tc (cadr outside-dif)) (map annotate-tc (caddr outside-dif))))
  			  									   		   				  ((or (equal? (car outside-dif) 'seq) (equal? (car outside-dif) 'or)) (list (car outside-dif) (handle-sequence-in-lambda (cdr outside-dif))))
  			  									   		   				  (else (annotate-tc outside-dif)))))))


(define pe->lex-pe
		(lambda (exp)
			;(newline)(display "exp in pe->lex-pe: ") (display exp) (newline)
			(if (not (equal? (car exp) 'define))
				(pe->lex-pe-pvar (pe->lex-pe-fvar (pe->lex-pe-bvar exp '()) '()))
				(let ((lex-pe (pe->lex-pe-pvar (pe->lex-pe-fvar (pe->lex-pe-bvar (cdr exp) '()) '()))))
					  (list (car exp) (car lex-pe) (caadr lex-pe))))))

(define annotate-tc
  	(lambda (exp)
  		(newline)(display "exp in annotate-tc: ") (display exp) (newline)
  		(cond ((or (not (list? exp)) (null? exp)) exp)
  			  ((and (list? exp) (= (length exp) 2) (equal? (car exp) 'const)) exp)
  			  ((equal? (car exp) 'lambda-simple) (list (car exp) (cadr exp) (create-tc-app (cddr exp))))
  			  ((equal? (car exp) 'lambda-opt) (list (car exp) (cadr exp) (caddr exp) (create-tc-app (cdddr exp))))
  			  ((and (equal? (car exp) 'applic)) (list (car exp) (annotate-tc (cadr exp)) (map annotate-tc (caddr exp))))
  			  ((and (equal? (car exp) 'if3) (equal? (caadr exp) 'if3)) (handle-if-in-test exp))
  			  ((equal? (car exp) 'if3) (let ((test (cadr exp))
  			  								 (dit (caddr exp))
  			  								 (dif (cadddr exp)))
  			  								 (list (car exp) (annotate-tc test) (cond ((equal? (car dit) 'applic) (list 'applic (annotate-tc (cadr dit)) (map annotate-tc (caddr dit))))
  			  									   							  		  ((or (equal? (car dit) 'seq) (equal? (car dit) 'or)) (list (car dit) (handle-sequence-in-lambda (cdr dit))))
  			  									   							  		  (else (annotate-tc dit)))
  			  									   								(cond ((equal? (car dif) 'applic) (list 'applic (annotate-tc (cadr dif)) (map annotate-tc (caddr dif))))
  			  									   							  		  ((or (equal? (car dif) 'seq) (equal? (car dif) 'or)) (list (car dif) (handle-sequence-in-lambda (cdr dif))))
  			  									   							  		  (else (annotate-tc dif))))))
  			  ((and (= (length exp) 2) (or (equal? (car exp) 'box) (equal? (car exp) 'box-get)))  exp)
  			  ((and (> (length exp) 1) (equal? (car exp) 'box-set)) (list (car exp) (cadr exp) (annotate-tc (cddr exp))))
  			  ((and (> (length exp) 1) (equal? (car exp) 'or)) (list (car exp) (map annotate-tc (cadr exp))))
  			  ((and (> (length exp) 1) (equal? (car exp) 'seq)) (list (car exp) (map annotate-tc (cadr exp))))
  			  ((and (> (length exp) 1) (not (list? (car exp))) (cons (car exp) (annotate-tc (cdr exp)))))
			  ((and (> (length exp) 1) (list? (car exp)) (list (annotate-tc (car exp)) (annotate-tc (cdr exp)))))
			  ((and (= (length exp) 1) (list? (car exp))) (annotate-tc (car exp)))
			  (else exp))))


;; parsed exp

'(define (var make-monitored)
  (lambda-simple
    (proc)
    (applic
      (lambda-simple
        (counter how-many-calls reset-count compute dispatch)
        (seq ((set (var counter) (const 0)) (set (var how-many-calls) (lambda-simple () (var counter)))
               (set (var reset-count)
                    (lambda-simple () (set (var counter) (const 0))))
               (set (var compute)
                    (lambda-simple
                      (arg)
                      (seq ((set (var counter)
                                 (applic
                                   (var +)
                                   ((var counter) (const 1))))
                             (applic (var proc) ((var arg)))))))
               (set (var dispatch)
                    (lambda-simple
                      (op arg)
                      (if3 (applic
                             (var eq?)
                             ((var op) (const how-many-calls?)))
                           (applic (var how-many-calls) ())
                           (if3 (applic
                                  (var eq?)
                                  ((var op) (const reset-count)))
                                (applic (var reset-count) ())
                                (if3 (applic
                                       (var eq?)
                                       ((var op) (const compute)))
                                     (applic (var compute) ((var arg)))
                                     (const 0))))))
               (applic (lambda-simple () (var dispatch)) ()))))
      ((const #f) (const #f) (const #f) (const #f) (const #f)))))

;; final result


'(def (fvar make-monitored)
     (lambda-simple
       (proc)
       (tc-applic
         (lambda-simple
           (counter how-many-calls reset-count compute dispatch)
           (seq ((set (pvar counter 0) (box (pvar counter 0)))
                  (set (pvar how-many-calls 1)
                       (box (pvar how-many-calls 1)))
                  (set (pvar reset-count 2) (box (pvar reset-count 2)))
                  (set (pvar compute 3) (box (pvar compute 3)))
                  (box-set (pvar counter 0) (const 0))
                  (box-set
                    (pvar how-many-calls 1)
                    (lambda-simple () (box-get (bvar counter 0 0))))
                  (box-set
                    (pvar reset-count 2)
                    (lambda-simple
                      ()
                      (box-set (bvar counter 0 0) (const 0))))
                  (box-set
                    (pvar compute 3)
                    (lambda-simple
                      (arg)
                      (seq ((box-set
                              (bvar counter 0 0)
                              (applic
                                (fvar +)
                                ((box-get (bvar counter 0 0)) (const 1))))
                             (tc-applic (bvar proc 1 0) ((pvar arg 0)))))))
                  (set (pvar dispatch 4)
                       (lambda-simple
                         (op arg)
                         (if3 (applic
                                (fvar eq?)
                                ((pvar op 0) (const how-many-calls?)))
                              (tc-applic
                                (box-get (bvar how-many-calls 0 1))
                                ())
                              ((if3 (applic
                                      (fvar eq?)
                                      ((pvar op 0) (const reset-count)))
                                    (tc-applic
                                      (box-get (bvar reset-count 0 2))
                                      ())
                                    (if3 (applic
                                           (fvar eq?)
                                           ((pvar op 0) (const compute)))
                                         (tc-applic
                                           (box-get (bvar compute 0 3))
                                           ((pvar arg 1)))
                                         (const 0)))))))
                  (pvar dispatch 4))))
         ((const #f) (const #f) (const #f) (const #f) (const #f)))))