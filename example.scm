(load "pc.scm")
(load "pattern-matcher.scm")
(load "topo.scm")

(define <end-of-line>
  (new
   
   (*parser (char #\newline))
   (*parser <end-of-input>)
   (*disj 2)
   
   done))

(define <line-comment>
  (new
   (*parser (char #\;))
   (*parser <any-char>)
   (*parser <end-of-line>)
   *diff
   *star
   
   (*parser <end-of-line>)
   (*caten 3)
  
   done))

(define <whitespace>
  (const
   (lambda (ch) (char<=? ch #\space))))

(define <expression-comment>
  (new

   (*parser (word "#;"))
   (*delayed (lambda () <infix-expression>))
   (*delayed (lambda () <sexpr>))
   (*disj 2)
   (*caten 2)
   (*pack-with (lambda (_c _e) '()))
   
   done)
  )

(define <skippable>
  (new

   (*parser <line-comment>)
   (*parser <expression-comment>)
   (*parser <whitespace>)
   (*disj 3)
   (*pack (lambda (e) '()))
   
   done))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new

       (*parser <wrapper>)
       (*parser <p>)
       (*parser <wrapper>)
       (*caten 3)
       (*pack-with (lambda (_left e _right) e))
       
       done))))

(define ^<skippable*>
  (^^<wrapped> (star <skippable>)))

(define <true>
  (new

   (*parser (word-ci "#t"))
   (*pack (lambda (true) #t))

   done))

(define <false>
  (new

   (*parser (word-ci "#f"))
   (*pack (lambda (false) #f))

   done))

(define <boolean>
  (new

   (*parser <true>)
   (*parser <false>)
   (*disj 2)
   
   done))

(define <char-prefix>
  (new

   (*parser (word "#\\"))

   done)
  )

(define <visible-simple-char>
  (new

   (*parser (const (lambda (char)
		     (char<? #\space char))))
   (*parser (const (lambda (char)
		     (char=? #\x7f char))))
   *diff
   

   done)
  )

(define <named-char>
  (new

   (*parser (word-ci "lambda"))
   (*pack (lambda (s) #\x3bb))
   (*parser (word-ci "newline"))
   (*pack (lambda (_) #\newline))
   (*parser (word-ci "nul"))
   (*pack (lambda (_) #\nul))
   (*parser (word-ci "page"))
   (*pack (lambda (_) #\page))
   (*parser (word-ci "return"))
   (*pack (lambda (_) #\return))
   (*parser (word-ci "space"))
   (*pack (lambda (_) #\space))
   (*parser (word-ci "tab"))
   (*pack (lambda (_) #\tab))
   (*disj 7)
   
   done)
  )

(define <hex-char>
  (new

   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\f))
   (*disj 2)
   *plus
   (*pack (lambda (s) (string->number (list->string s) 16)))
   
   done))

(define <hex-unicode-char>
  (new

   (*parser (char #\x))
   (*parser <hex-char>)
   ;*plus
   (*caten 2)
   (*pack (lambda (s) (integer->char (cadr s))))
   done)
  )


(define <char>
  (new

   (*parser <char-prefix>)
   (*parser <named-char>)
   (*parser <hex-unicode-char>)
   (*parser <visible-simple-char>)
   (*disj 3)
   (*caten 2)
   (*pack (lambda (s) (cadr s)))

  done))

(define <+->
  (new

   (*parser (char #\+))
   (*pack (lambda (s) +))
   (*parser (char #\-))
   (*pack (lambda (s) -))
   (*disj 2)

   done)

  )

(define <0-9>
  (new

   (*parser (range #\0 #\9))

   done))

(define <1-9>
  (new

   (*parser (range #\1 #\9))

   done))

(define <natural>
  (new

   (*parser <0-9>)
   *plus
   (*pack (lambda (n) (string->number (list->string n))))
   

   done)
  )

(define <integer>
  (new

   (*parser <natural>)
   (*parser <+->)
   (*parser <natural>)
   (*caten 2)
   (*pack (lambda (n) ((car n) (cadr n))))
   (*disj 2)
   (*pack (lambda (n) n))
   done)

  )

(define <fraction>
  (new

   (*parser <integer>)
   (*parser (char #\/))
   (*parser <natural>)
   (*caten 3)
   (*pack-with (lambda (left frac right) (/ left right)))

   done)
  )

(define <number>
  (^<skippable*>
  (new

   (*parser <fraction>)
   (*parser <integer>)
   (*disj 2)

   done))
  )

(define <string-meta-char>
  (new

   (*parser (word "\\t"))
   (*pack (lambda (s) "\t"))
   (*parser (word "\\f"))
   (*pack (lambda (s) "\f"))
   (*parser (word "\\n"))
   (*pack (lambda (s) "\n"))
   (*parser (word "\\r"))
   (*pack (lambda (s) "\r"))
   (*disj 4)
   
   (*parser (word "\\\\"))
   (*pack (lambda (s) "\\"))
   (*parser (word "\\\""))
   (*pack (lambda (s) "\""))
   (*disj 3)

   done))

(define <string-hex-char>
  (new

   (*parser (word "\\x"))
   (*parser <hex-char>)
   (*parser (char #\;))
   (*caten 3)
   (*pack-with (lambda (pref char-star semi-colon) (string (integer->char char-star))))
   
  done))

(define <string-literal-char>
  (new

   (*parser <any-char>)
   (*parser (char #\"))
   *diff
   (*parser (char #\\))
   *diff
   (*pack (lambda (s) (string s)))

  done))

(define <string-char>
  (new

   (*parser <string-meta-char>)
   (*parser <string-hex-char>)
   (*parser <string-literal-char>)
   (*disj 3)

   done))


(define <string>
  (new

   (*parser (char #\"))
   (*parser <string-char>)
   *star
   (*parser (char #\"))
   (*caten 3)
   (*pack-with (lambda (fslash s lslash) (fold-right
					  string-append
					  ""
					  s
					  )))

   done))

(define <symbol-char>
  (new

   (*parser (range-ci #\a #\z))
   (*parser (range #\0 #\9))
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\^))
   (*parser (char #\*))
   (*parser (char #\-))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\+))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
   (*parser (char #\/))
   (*disj 14)
   
   done))

(define <symbol-tag>
  (new

   (*parser <symbol-char>)
   *plus
   (*pack (lambda (c) (string-downcase (list->string c))))
   
   done))

(define <proper-list>
  (new

   (*parser (char #\())
   (*delayed (lambda () <sexpr>))
   *star
   (*pack (lambda exps exps))
   (*parser (char #\)))
   (*caten 3)
   (*pack-with (lambda (_leftb list-exp _rightb) (car list-exp)))
   
   done))

(define <improper-list>
  (new

   (*parser (char #\())
   (*delayed (lambda () <sexpr>))
   *plus
   (*parser (char #\.))
   (*delayed (lambda () <sexpr>))
   (*parser (char #\)))
   (*caten 5)
   (*pack-with (lambda (brac-left list-expr dot exp brac-right) (append list-expr exp)))

   done))

(define <vector>
  (new

   (*parser (char #\#))
   (*parser <proper-list>)
   (*caten 2)
   (*pack-with (lambda (_ lst) (list->vector lst)))
   
   done))

(define <quoted>
  (new

   (*parser (char #\'))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with (lambda (quo s) `',s))
   
   done))

(define <quasi-quoted>
  (new

   (*parser (char #\`))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with (lambda (quasi s) (list `quasiquote `,s)))

   done))

(define <unquoted>
  (new

   (*parser (char #\,))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with (lambda (unqou s) (list 'unquote `,s)))

   done))

(define <unquote-and-spliced>
  (new

   (*parser (word ",@"))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with (lambda (qutosplice s) (list 'unquote-splicing `,s)))

   done))

(define eval-preced (lambda (op1 op2)
		      (cond ((eq? op1 'expt) #t)
			    ((or (eq? op1 '*) (eq? op1 '/)) (if (eq? op2 'expt) #f #t))
			    (else (if (or (eq? op2 'expt) (eq? op2 '*) (eq? op2 '/)) #f #t)))))

(define arith-op? (lambda (op)
		    (and (symbol? op)
			 (or (symbol=? op '+)
			     (symbol=? op '-)
			     (symbol=? op '*)
			     (symbol=? op '/)
			     (symbol=? op 'expt)
			     ))))

(define infix->prefix (lambda (number op rest)
			(let ((op-first op)
			      (op-second (car rest))
			      (left-rest (if (or (null? (cdr rest)) (not (pair? (cdr rest)))) (cdr rest) (cadr rest))))
			  (cond ((null? left-rest) (list op-first number op-second))
				((eq? op-first 'braced) (cons 'braced rest))
				((and (symbol? op-second) (symbol=? op-second 'braced)) (list op-first number rest))
				((arith-op? op-second)
				 (cond ((and (symbol? op-first) (symbol? op-second) (symbol=? op-first op-second 'expt)) (list 'expt number (append (list 'expt) (cdr rest))))
				       ((eval-preced op-first op-second) (if (or (symbol? left-rest) (number? left-rest))
									     (list op-second (list op-first number left-rest) (caddr rest))
									     (list op-second (infix->prefix number op-first left-rest) (caddr rest))))
				       (else (list op-first number rest))
				       ))
				(else (list op-first number rest))
				))))
									     
  
(define packed-op (lambda (left-exp op rest)
		    (let ((left-exp (if (and (list? left-exp) (eq? (car left-exp) 'braced) #f) (cdr left-exp) left-exp)))
		      (if (and (list? rest) (eq? 'braced op)) (cdr rest)
			  (if (and (list? rest) (eq? 'braced (car rest)) #f) (list op left-exp (cdr rest))
			      (if (or (number? rest) (symbol? rest)) (list op left-exp rest)
				  (infix->prefix left-exp op rest)))))))

(define <infix-prefix-extension-prefix>
  (new

   (*parser (word "##"))
   (*parser (word "#%"))
   (*disj 2)

   done)
  )

(define <infix-add>
  (^<skippable*>
   (new

    (*delayed (lambda () <infix-terminating>))
    (*parser (char #\+))
    (*delayed (lambda () <infix-expression>))
    (*caten 3)
    (*pack-with (lambda (left-exp op+ rest) (packed-op left-exp '+ rest)))

    done)))

(define <infix-neg>
  (new

   (*parser (char #\-))
   (*delayed (lambda () <infix-terminating>))
   (*caten 2)
   (*pack-with (lambda (neg-sign s) (cond ((symbol? s) (append (list 'braced) (list '- s)))
					  ((number? s) (append (list 'braced) (- 0 s)))
					  #;(else (let ((left-term (if (pair? (car s))
								     (cadr s)
								     (cdr s)) )
						      (exp-op (car s))
						      (rest (if (pair? (cdr s))
								(cddr s)
								'()) ) )
						  (if (eq? exp-op 'braced)
						      (list 'braced '- s)
						      (append (list exp-op (append (list 'braced) (list '- left-term))) rest))  ; complete neg-ing
					  ))
					  (else (list '- s))

					  )))
   done)
  )

(define <infix-sub>
  (new

   (*delayed (lambda () <infix-terminating>))
   (*parser (char #\-))
   (*delayed (lambda () <infix-expression>))
   (*caten 3)
   (*pack-with (lambda (left-exp op- rest) (packed-op left-exp '- rest)))

   done)
  )

(define <infix-mul>
  (new

   (*delayed (lambda () <infix-terminating>))
   (*parser (char #\*))
   (*delayed (lambda () <infix-expression>))
   (*caten 3)
   (*pack-with (lambda (left-exp op+ rest) (packed-op left-exp '* rest)))

   done)
   )

(define <infix-div>
  (new

   (*delayed (lambda () <infix-terminating>))
   (*parser (char #\/))
   (*delayed (lambda () <infix-expression>))
   (*caten 3)
   (*pack-with (lambda (left-exp op/ rest) (packed-op left-exp '/ rest)))

   done)
  )

(define <infix-pow>
  (new

   (*delayed (lambda () <infix-terminating>))
   (*parser (word "**"))
   (*parser (char #\^))
   (*disj 2)
   (*delayed (lambda () <infix-expression>))
   (*caten 3)
   (*pack-with (lambda (left-exp op-pow rest) (packed-op left-exp 'expt rest))) 

   done)
  )

(define <infix-arith-op>
  (new

   (*parser (char #\+))
   (*pack (lambda (_) '+))
   (*parser (char #\-))
   (*pack (lambda (_) '-))
   (*parser (char #\*))
   (*pack (lambda (_) '*))
   (*parser (char #\/))
   (*pack (lambda (_) '/))
   (*parser (char #\^))
   (*parser (word "**"))
   (*disj 2)
   (*pack (lambda (_) 'expt))
   (*disj 5)
   
   done))

(define <infix-expression-single>
  (new

      
   (*delayed (lambda () <infix-terminating>))
   (*parser <infix-arith-op>)
   (*delayed (lambda () <infix-expression>))
   (*caten 3)
   (*pack-with (lambda (left-term op-sign right-exp) (packed-op left-term op-sign right-exp)))
   (*delayed (lambda () <infix-terminating>))
   (*disj 2)
   
   done))

(define remove-braced (lambda (expr)
			(cond ((null? expr) '())
			      ((and (pair? expr) (symbol? (car expr)) (symbol=? (car expr) 'braced)) (cdr expr))
			      (else expr))))

(define <infix-array-deref*>
  (new

   (*parser (char #\[))
   (*delayed (lambda () <infix-expression>))
   (*parser (char #\]))
   (*caten 3)
   (*pack-with (lambda (_l expr _r) (list (remove-braced expr))))
   
   done))

(define <infix-array-get>
  (new

   (*delayed (lambda () <array-infix-expr>))
   (*parser (char #\[))
   (*delayed (lambda () <infix-expression>))
   (*parser (char #\]))
   (*caten 4)
   (*pack-with (lambda (vec _l deref-expr _r) (list 'vector-ref (remove-braced vec) (remove-braced deref-expr))))
   (*parser <infix-array-deref*>)
   (*pack (lambda (derefs) derefs))
   *star
   (*caten 2)
   (*pack-with (lambda (first rest) (fold-left (lambda (vec deref) (list 'vector-ref vec (car deref))) first rest)))
   
   
   done))

(define <infix-arglist>
  (new

   (*parser (char #\())

   (*delayed (lambda () <infix-expression>))
   (*parser (char #\,))
   (*delayed (lambda () <infix-expression>))
   (*caten 2)
   (*pack-with (lambda (_ exprs?) exprs?))
   *star
   (*caten 2)
   (*pack-with (lambda (first-expr rest-exprs?) (cons first-expr rest-exprs?)))
   (*parser (char #\)))
   (*caten 3)
   (*pack-with (lambda (_a exp _b) exp))
   (*parser (char #\())
   (*parser (^<skippable*> <epsilon>))
   (*parser (char #\)))
   (*caten 3)
   (*pack-with (lambda (_a letreik _b) '()))
   (*disj 2)
   
   done))

(define <infix-funcall>
  (new

   (*delayed (lambda () <funcall-infix-expr>))
   (*parser <infix-arglist>)
   (*caten 2)
   (*pack-with (lambda (func_name args ) (append (list func_name) args)))
   
   done))

(define <infix-paren>
  (new

   (*parser (char #\())
   (*delayed (lambda () <infix-expression>))
   (*parser (char #\)))
   (*caten 3)
   (*pack-with (lambda (left-brac expr right-brac) (cons 'braced expr)))
   
   done)
  )

(define <infix-sexpr-escape>
  (new

   (*parser <infix-prefix-extension-prefix>)
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with (lambda (_ sexpr) (cons 'braced sexpr)))
   
   done))

(define <infix-symbol-tag>
  (new

   (*parser <symbol-char>)
   (*parser (char #\+))
   (*parser (char #\-))
   (*parser (char #\*))
   (*parser (char #\/))
   (*parser (word "**"))
   (*parser (char #\^))
   (*disj 6)
   *diff
   *plus
   (*pack (lambda (c) (string-downcase (list->string c))))

   done))

(define <funcall-infix-expr>
  (new

   (*delayed (lambda () <infix-symbol>))
   (*parser <number>)
   *diff
   (*parser <infix-paren>)
   (*parser <infix-sexpr-escape>)
   (*disj 3)

   done)
  )

(define <array-infix-expr>
  (new

   (*delayed (lambda () <infix-symbol>))
   (*parser <number>)
   *diff
   (*parser <infix-paren>)
   (*parser <funcall-infix-expr>)
   (*disj 3)

   done)
  )

(define <infix-deref>
  (new
   (*parser (char #\[))
   (*delayed (lambda () <infix-expression>))
   (*parser (char #\]))
   (*caten 3)
   (*pack-with (lambda (_a exp _b) (cons 'vec exp)))
   (*parser <infix-arglist>)
   (*pack (lambda (args) (cons 'args args)))
   (*disj 2)
   *plus
   done))

(define <infix-getorcall>
  (new
   (*parser (^<skippable*>
	     (new
	      (*parser <infix-sexpr-escape>)
	      (*parser <infix-paren>)
	      (*delayed (lambda () <infix-symbol>))
	      (*parser <number>)
	      (*disj 4)
	     done)
   ))
   (*parser <infix-deref>)
   (*caten 2)
   (*pack-with (lambda (id refs-or-args)
		 (letrec ((run (lambda (ide next)
				 (cond ((null? next) ide)
				       ((eq? (car next) 'args) (append (list ide) (cdr next)))
				       (else (list 'vector-ref ide (cdr next)))))))
		   (fold-left run id refs-or-args))))
   done))

(define <infix-terminating>
  (^<skippable*>
   (new

    ;(*parser <infix-funcall>)
    ;(*parser <infix-array-get>)
    (*parser <infix-getorcall>)
    (*parser <infix-sexpr-escape>)
    (*delayed (lambda () <infix-symbol>))
    (*parser <number>)
    
    (*parser <infix-paren>)
    (*parser <infix-neg>)

    
    (*disj 6)
    
    done)))

(define <infix-expression>

  (^<skippable*>
   (new


    
    (*parser <infix-sub>)
    (*parser <infix-div>)
    (*parser <infix-pow>)
    (*parser <infix-add>)
    (*parser <infix-mul>)

    (*parser <infix-getorcall>)
    (*parser <infix-paren>)
    ;(*parser <infix-array-get>)
    ;(*parser <infix-funcall>)

    
    (*parser <infix-neg>)
    (*parser <infix-sexpr-escape>)

    
    (*delayed (lambda () <infix-symbol>))
    (*parser <number>)

    
    (*disj 11)
    
    done))
  )

(define <infix-extension>
  (new

   (*parser <infix-prefix-extension-prefix>)
   (*parser <infix-expression>)
   (*caten 2)
   (*pack-with (lambda (infix-sign expr) expr))

   done))

(define <symbol> (new

   (*parser <symbol-tag>)
   (*parser <number>)
   *diff
   (*pack (lambda (not-a-num) (string->symbol not-a-num)))
   (*parser <+->)
   *maybe
   (*parser <0-9>)
   *plus
   (*pack (lambda (num) (list->string num)))
   (*delayed (lambda () <symbol>))
   *plus
   (*caten 3)
   (*pack-with (lambda (sign? num rest)
		 (string->symbol (string-append (if (car sign?)
						    (cond ((eq? (cadr sign?) +) "+")
							  ((eq? (cadr sign?) -) "-")
							  )
						    "")
						num
						(if (pair? rest)
						    (if (symbol? (car rest))
							(symbol->string (car rest))
							(car rest) )
						    (if (null? rest)
							""
							rest)) )
		  )
		 ))
   (*parser <fraction>)
   *diff
   (*disj 2)
   
   *plus
   (*pack (lambda (e) (if (null? e)
			  '()
			  (car e))))
   
   done))

(define <infix-symbol> (new

   (*parser <infix-symbol-tag>)
   (*parser <number>)
   *diff
   (*pack (lambda (not-a-num) (string->symbol not-a-num)))
   (*parser <0-9>)
   *plus
   (*pack (lambda (num) (list->string num)))
   (*delayed (lambda () <infix-symbol>))
   *plus
   (*caten 2)
   (*pack-with (lambda (num rest)
		 (string->symbol (string-append num
						(if (pair? rest)
						    (if (symbol? (car rest))
							(symbol->string (car rest))
							(car rest) )
						    (if (null? rest)
							""
							rest) ) )
		  )
		 ))
   (*disj 2)
   
   *plus
   (*pack (lambda (e) (if (null? e)
			  '()
			  (car e))))
   
   done))

(define <sexpr>
  (^<skippable*>
   (new

    (*parser <boolean>)
    (*parser <char>)

    (*parser <symbol>)
    (*parser <number>)
    (*parser <string>)
    
    (*parser <proper-list>)
    (*parser <improper-list>)
    (*parser <vector>)
    (*parser <quoted>)
    (*parser <quasi-quoted>)
    (*parser <unquoted>)
    (*parser <unquote-and-spliced>)
    (*parser <infix-extension>)

    (*disj 13)
    #;(*pack (lambda (expr)
	     (if (and (list? expr) (not (null? expr)))
		 (if (and (symbol? (car expr)) (symbol=? (car expr) 'braced)) (map (lambda (inner-exp) (if (and (pair? inner-exp) (symbol=? (car inner-exp) 'braced))
										(cdr inner-exp)
										inner-exp))
							(cdr expr))
		     (map
			       (lambda (inner-exp) (if (and (pair? inner-exp) (symbol? (car inner-exp)) (symbol=? (car inner-exp) 'braced))
						       (cdr inner-exp)
						       inner-exp))
			       expr))
		 (if (and (pair? expr) (symbol? (car expr)) (symbol=? (car expr) 'braced))
		     (cdr expr)
		     expr))))
    (*pack (letrec ((runrec (lambda (exp)
			      (if (list? exp)
				  (begin (set! exp (remove-braced exp)) (map runrec exp))
				  (remove-braced exp)))))
	     (lambda (expr)
	       (runrec (runrec expr)))))
    done)))
    
;;;;;;;;;;;;;;;;;;;;;;;; HW2 ;;;;;;;;;;;;;;;;;;;;


(define *reserved-words* '(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define *core-forms* '(const var if3 or lambda-simple lambda-var lambda-opt def set applic seq))

(define reserved? (lambda (var)
		    (if (symbol? var)
		    (ormap (lambda (word) (symbol=? var word))
			   *reserved-words*)
		    #f)))

(define rec-check-reserved
  (lambda (e)
    (if (list? e) (andmap (lambda (e2) (rec-check-reserved e2)) e)
	(not (reserved? e)))))

(define rule-quoted-const (pattern-rule
		    `(quote ,(? 'con))
		    (lambda (con) `(const ,con))))

(define rule-regular-const (pattern-rule
			  `,(? 'con (lambda (e) (not (null? e))) rec-check-reserved)
			  (lambda (con) `(const ,con))))

(define rule-const (compose-patterns
		    rule-quoted-const
		    rule-regular-const))

(define rule-var (pattern-rule
		  `,(? 'var symbol? (lambda (e) (not (reserved? e))))
		  (lambda (var) `(var ,var))))

(define rule-conditional-full (pattern-rule
			       `(if ,(? 'test) ,(? 'then) ,(? 'else))
			       (lambda (test then else)
				 `(if3 ,(parse test) ,(parse then) ,(parse else)))
			       ))

(define rule-conditional-no-else (pattern-rule
				  `(if ,(? 'test) ,(? 'then))
				  (lambda (test then)
				    `(if3 ,(parse test) ,(parse then) ,(parse (void))))))
			  
(define rule-conditional (compose-patterns
			  rule-conditional-full
			  rule-conditional-no-else))

(define rule-disj-not-empty (pattern-rule
			     `(or ,(? 'expr) . ,(? 'exprs))
			     (lambda (expr exprs)
			       (if (null? exprs)
				   (parse expr)
				   `(or ,(cons (parse expr) (map (lambda (e) (parse e)) exprs))))
			       )
			     ))

(define rule-disj-empty (pattern-rule
			 `(or)
			 (lambda () (parse #f))
			 ))

(define rule-disj (compose-patterns
		   rule-disj-empty
		   rule-disj-not-empty
		   ))

(define rule-applic (pattern-rule
		     `,(? 'applic (lambda (app) (and (list? app)(not (reserved? (car app))))))
		     (lambda (applic)
		       `(applic ,(parse (car applic)) ,(map (lambda (e) (parse e)) (cdr applic))))))

(define duplicate-keys?
  (lambda (keys)
    (if (null? keys) #f
	(if (not (member (car keys) (cdr keys))) (duplicate-keys? (cdr keys)) #t)))
  )

(define rule-lambda-simple-empty (pattern-rule
				  `(lambda () ,(? 'body) . ,(? 'bodies-rest))
				  (lambda (body bodies-rest)
				    `(lambda-simple () ,(parse `(begin ,body ,@bodies-rest))))
				    
				  ))

(define rule-lambda-simple-none-empty (pattern-rule
				       `(lambda ,(? 'params (lambda (params) (and (list? params) (andmap
									      (lambda (e)
										(not (reserved? e)))
									      params) )))
					  ,(? 'body) . ,(? 'bodies-rest))
				       (lambda (params body rest)
					 (if (duplicate-keys? params) "ERROR"
					 `(lambda-simple ,params
							   ,(parse `(begin ,body ,@rest)))))))

(define is-safe?
  (lambda (params)
    (if (or (not (pair? (cdr params))) (null? (cdr params))) (not (or (reserved? (car params)) (reserved? (cdr params))))
	(if (not (reserved? (car params))) (is-safe? (cdr params)) #f))))

(define rule-lambda-opt (pattern-rule
			 `(lambda ,(? 'params pair? is-safe?)
			    ,(? 'body) . ,(? 'bodies-rest)
			    )
			 (lambda (params body bodies-rest)
			   (letrec ((un-listify (lambda (imp-list)
						  (if (pair? (cdr imp-list))
						      (un-listify (cdr imp-list))
						      (cdr imp-list)
						      )
						  )
						)
				    (listify (lambda (imp-list lst)
					       (if (pair? (cdr imp-list))
						   (listify (cdr imp-list) (append lst (list (car imp-list))))
						   (append lst (list (car imp-list)))
						   )
					       )
					     ))
			     (if (duplicate-keys? (listify params (list (un-listify params)))) "ERROR"
			     `(lambda-opt ,(listify params '()) ,(un-listify params)
					  ,(parse `(begin ,body ,@bodies-rest)))
			     
			     )
			   )
			 )
  ))

(define rule-lambda-simple (compose-patterns
			    rule-lambda-simple-empty
			    rule-lambda-simple-none-empty
			     ))

(define rule-lambda-var (pattern-rule
			 `(lambda ,(? 'var-args (lambda (e) (not (reserved? e))))
			    ,(? 'body) . ,(? 'bodies-rest))
			 (lambda (var-args body bodies-rest)
			   `(lambda-var ,var-args
					,(parse `(begin ,body ,@bodies-rest))))
			 )
  )

(define rule-lambda-form (compose-patterns
			  
			  rule-lambda-simple
			  rule-lambda-opt
			  rule-lambda-var
			  ))

(define rule-define-reg (pattern-rule
			 `(define ,(? 'var (lambda (e) (not (or (pair? e) (list? e) (reserved? e)))))
			    ,(? 'value) . ,(? 'values))
			 (lambda (var val vals)
			   `(def ,(parse var) ,(parse `(begin ,val ,@vals))))))

(define rule-define-mit (pattern-rule
			 `(define ,(? 'var (lambda (e) (not (reserved? e))))
			    ,(? 'value) . ,(? 'values))
			 (lambda (var val vals)
			   `(def ,(parse (car var)) ,(parse `(lambda ,(cdr var) (begin ,val ,@vals)))))
			 ))

(define rule-define-form (compose-patterns
			  rule-define-reg
			  rule-define-mit
			  ))

(define rule-assign (pattern-rule
		     `(set! ,(? 'var) ,(? 'expr))
		     (lambda (var expr)
		       `(set ,(parse var) ,(parse expr))
		       )
		     ))

(define rule-seq-empty (pattern-rule
			`(begin)
			(lambda () (parse (void)))
			)
  )

(define seq-helper (pattern-rule
		    `(seq ,(? 'expr))
		    (lambda (expr) expr)
		     )
  )

(define rule-seq (pattern-rule
		  `(begin ,(? 'expr) . ,(? 'exprs))
		  (lambda (expr exprs)
		    (if (null? exprs)
			(parse expr)
			`(seq (,@(fold-right append '() (map (lambda (e) (seq-helper e (lambda () (list e)))) (map parse (append (list expr) exprs)))))
			      ))
		    ))
  )

(define macro-empty-let (pattern-rule
			 `(let () ,(? 'body) . ,(? 'bodies))
			 (lambda (body bodies)
			   (parse `((lambda () ,body ,@bodies))))))

(define macro-let (pattern-rule
		   `(let ((,(? 'key (lambda (e) (not (reserved? e)))) ,(? 'value)) . ,(? 'other)) ,(? 'body) ., (? 'body-rest))
		   (lambda (key value other body body-rest)
		     (let ((keys (if (null? other)
				     (list key)
				     (append (list key) (map car other))))
			   (values (if (null? other)
				       (list value)
				       (append (list value) (map cadr other))))
			   (body (if (null? body-rest) (list body) (append (list body) body-rest))))
		      (if (duplicate-keys? keys) "ERROR" (parse `((lambda ,keys ,@body) ,@values))
		       )
		      ))))


(define expand-let (pattern-rule
		   `(let ((,(? 'key (lambda (e) (not (reserved? e)))) ,(? 'value)) . ,(? 'other)) ,(? 'body) ., (? 'body-rest))
		   (lambda (key value other body body-rest)
		     (let ((keys (if (null? other)
				     (list key)
				     (append (list key) (map car other))))
			   (values (if (null? other)
				       (list value)
				       (append (list value) (map cadr other))))
			   (body (if (null? body-rest) (list body) (append (list body) body-rest))))
		      (if (duplicate-keys? keys) "ERROR" (parse-after-parse `((lambda-simple ,keys ,@body) ,@values))
		       )
		     ))))

(define macro-empty-let* (pattern-rule
			  `(let* () ,(? 'body) . ,(? 'bodies))
			  (lambda (body bodies)
			    (parse `((lambda () ,body ,@bodies))))))

(define macro-let* (pattern-rule
		    `(let* ((,(? 'key (lambda (e) (not (reserved? e)))) ,(? 'value)) . ,(? 'other)) ,(? 'body) . ,(? 'body-rest))
		    (lambda (key value other body body-rest)
		      (if (null? other)
			  (parse `(let ((,key ,value)) ,@(append (list body) body-rest)))
			  (parse `(let ((,key ,value)) (let* ,other ,@(append (list body) body-rest)))))
		      )))

(define set-false
  (lambda (key other)
    (let ((keys (append (list key) (map (lambda (e) (car e)) other))))
      (map (lambda (key) (list key #f)) keys))))

(define get-lambda-body
  (lambda (body body-rest)
    (if (null? body-rest) (list body) `(,body ,@body-rest))))

(define macro-letrec (pattern-rule
		      `(letrec ((,(? 'key (lambda (e) (not (reserved? e)))) ,(? 'value)) . ,(? 'other)) ,(? 'body) . ,(? 'body-rest))
		      (lambda (key value other body body-rest)
			 (parse `(let ,(set-false key other) (begin (set! ,key ,value)
								     ,@(map (lambda (e) `(set! ,(car e)
											    ,(cadr e)))other)
								     ((lambda ()
									,@(get-lambda-body body body-rest))))
				      )))))

(define expand-letrec (pattern-rule
		      `(letrec ((,(? 'key (lambda (e) (not (reserved? e)))) ,(? 'value)) . ,(? 'other)) ,(? 'body) . ,(? 'body-rest))
		      (lambda (key value other body body-rest)
			 (parse-after-parse `(let ,(set-false key other) (seq (set! ,key ,value)
								     ,@(map (lambda (e) `(set! ,(car e)
											    ,(cadr e)))other)
								     ((lambda ()
									,@(get-lambda-body body body-rest))))))
					)))


(define macro-empty-letrec
  (pattern-rule `(letrec () ,(? 'body) . ,(? 'body-rest))
		(lambda (body body-rest)
		  (parse `(let () ( (lambda() ,@(get-lambda-body body body-rest))))))))

(define rule-let* (compose-patterns
		   macro-empty-let*
		   macro-let*))

(define rule-let (compose-patterns
		  macro-empty-let
		  macro-let))

(define rule-letrec (compose-patterns
		     macro-empty-letrec
		     macro-letrec))

(define rule-empty-and (pattern-rule
			`(and)
			(lambda ()
			  (parse `#t))))

  (define rule-and-nonempty (pattern-rule
		  `(and ,(? 'first) . ,(? 'rest))
		  (lambda (first rest)
		    (cond ((null? rest) (parse first))
			  ( (null? (cdr rest)) (parse `(if ,first ,(car rest) #f)))
			  (else (parse `(if ,first (and ,(car rest) ,@(cdr rest))#f)))))))

(define rule-and (compose-patterns
		  rule-empty-and
		  rule-and-nonempty))	

(define rule-cond (pattern-rule
		   `(cond ,(? 'first) . ,(? 'rest))
		   (lambda (first rest)
		     (cond ((null? rest) (if (and (not (list? (car first))) (symbol=? 'else (car first)))
					     (parse `(begin ,@(cdr first)))(parse `(if ,(car first) (begin ,@(cdr first))))))
			   ((null? (cdr rest)) (parse `(if ,(car first) (begin ,@(cdr first)) (cond ,(car rest)))))
			   (else (parse `(if ,(car first) (begin ,@(cdr first))
					       (cond ,(car rest) ,@(cdr rest)))))))))

(define rule-qq (pattern-rule
		 `(quasiquote . ,(? 'expr))
		 (lambda (expr)
		   (let ((qqe (expand-qq (car expr))))
		    (parse qqe))
		   )
		 ))

(define core-form? (lambda (e)
		     (member e '(const var or if3 lambda-simple lambda-opt lambda-var def applic set seq set-box get-box))))

(define parse-after-parse
    (let ((run (compose-patterns
	      rule-applic
	      rule-conditional
	      rule-disj
	      rule-lambda-form
	      rule-define-form
	      rule-assign
	      rule-seq-empty
	      rule-seq
	      rule-let
	      rule-let*
	      rule-letrec
	      rule-and
	      rule-cond
	      rule-qq
	      rule-var
	      rule-const

	      )))
    (lambda (sexpr)
      (if (core-form? sexpr)
	  sexpr
	  (run sexpr
	       (lambda () "parse-after-parse"))))))

(define parse
  (let ((run (compose-patterns
	      rule-applic
	      rule-conditional
	      rule-disj
	      rule-lambda-form
	      rule-define-form
	      rule-assign
	      rule-seq-empty
	      rule-seq
	      rule-let
	      rule-let*
	      rule-letrec
	      rule-and
	      rule-cond
	      rule-qq
	      rule-var
	      rule-const

	      )))
    (lambda (sexpr)
	  (run sexpr
	       (lambda () "parse-after-parse")))))
;;; qq.scm
;;; A naive, one-level quasiquote implementation + optimizations
;;;
;;; Programmer: Mayer Goldberg, 2016


(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))


;;;;;;;;;;;;;;;;; HW3 ;;;;;;;;;;;;;;;;;;;;;;;;

(define is-lambda?
  (lambda (exp)
    (if (and (symbol? exp) (or (symbol=?  exp 'lambda-simple) (symbol=? exp 'lambda-opt)
			       (symbol=? exp 'lambda-var)))
	#t
	(if (list? exp) (is-lambda? (car exp)) #f))))

(define extract-lambda
  (lambda (exp)
    (if (symbol? (car exp))
	(list (caddr exp))
	(extract-lambda (car exp)))))

(define format-letrec
  (lambda (defs exps)
    (let ((def-names
	    (map (lambda (exp) (cadr exp)) defs))
	  (defs-values
	    (map (lambda (exp) (cddr exp)) defs))
	  (falses
	   (map (lambda (exp) (parse #f)) defs))
	  (sets
	   (map (lambda (exp) `(set ,(cadr exp) ,(cddr exp))) defs)))
      `((lambda-simple ,(map (lambda (e) (cadr e)) def-names)
		       (seq ,sets ,exps))
	,falses))))

(define remove-const
  (lambda (letrec-expr)
    (let ((lambda-body (extract-lambda letrec-expr)))
      (set-car! lambda-body 'hi)
      (list 'lambda-simple lambda-body))
    ))

(define fix-parsed-letrec
  (lambda (exp)
    (let ((fix-args
	   (set-car! (cdr exp) (cdar(cdaadr exp))))
	  )
      exp)))

(define letrec-after-parse
  (lambda (tag vars defs exps)
    (let ((parsed-exps `(quote ,@exps))
	  (letrec-ribs (map (lambda (def) (list `(quote ,(cadr def)) `(quote ,@(cddr def)))) defs)))
      (let* ((ayy (parse `(letrec ,letrec-ribs ,parsed-exps)))
	     (letrecyfied (fix-parsed-letrec (cadr ayy)))
				       )
	`(,tag ,vars ,letrecyfied ....... ,ayy)
	#;(parse `(letrec ,letrec-ribs ,parsed-exps))))))

(define parse-new
  (lambda (defs rest)
    (let ((params (map (lambda (x) (cadadr x)) defs))
	  (falses (map (lambda (x) `(const #f)) defs))
	  (seqs (map (lambda (x) `(set (var ,(cadadr x)) ,@(cddr x))) defs)))
      `(applic (lambda-simple ,params ,(eliminate-nested-defines `(seq (,@seqs ,@rest)))) ,falses))))

(define eliminate-nested-defines
  (lambda (exp)
    (cond ((and (pair? exp) (lambda-form? (car exp))) (let ((lambda-body
							     (if (eq? (car exp) 'lambda-opt) (cdddr exp) (cddr exp)))
							    (ans '()))
							#;(end$ lambda-body (lambda (e s) `(,(car exp) ,(cadr exp) ,(parse-new e s))))
							(end$ lambda-body (lambda (e s)
									    (if (null? e)
										(begin (set! ans (map eliminate-nested-defines s)) (if (< 1 (length ans)) (set! ans (list `(seq ,ans))))
										       (if (eq? (car exp) 'lambda-opt)
											   `(,(car exp) ,(cadr exp) ,(caddr exp) ,@ans)
											   `(,(car exp) ,(cadr exp) ,@ans)))
										(if (eq? (car exp) 'lambda-opt)
										    (begin (set! ans (parse-new e s)) `(,(car exp) ,(cadr exp) ,(caddr exp) ,ans))
										    (begin (set! ans (parse-new e s)) `(,(car exp) ,(cadr exp) ,ans))))))
							))
	  ((list? exp) (map (lambda (x) (eliminate-nested-defines x)) exp))
	  (else exp))))

(define end$
  (lambda (parse-expr ret)
   (if (null? parse-expr) (ret '() '())
	(end$ (cdr parse-expr)
	      (lambda (defs exps)
		(cond ((eq? (caar parse-expr) 'def) (ret (cons (car parse-expr) defs) exps))
		      ((eq? (caar parse-expr) 'seq)
		       (end$ (cadar parse-expr)
			     (lambda (defs1 exps1)
			       (ret (append defs1 defs)
				    (append exps1 exps)))))
		      (else (ret defs (cons (car parse-expr) exps)))))))))

(define applic-lambda
  (pattern-rule
   `(applic (lambda-simple () ,(? 'body)) ())
   (lambda (body) (remove-applic-lambda-nil body))))

(define applic-else
  (pattern-rule
   `(applic ,(? 'func-name) ,(? 'args))
   (lambda (func-name args)
     `(applic ,func-name ,args))))

(define remove-applic-lambda-nil
  (lambda (parsed-expr)
    (let ((rule
	   (compose-patterns
	    applic-lambda
	    #;applic-else)))
      (rule parsed-expr
	    (lambda () 
			  (let ((left (cond ((or (null? parsed-expr) (null? (car parsed-expr))) '()) ((list? (car parsed-expr)) (remove-applic-lambda-nil (car parsed-expr))) (else (car parsed-expr))))
				(right (cond ((null? parsed-expr) '()) ((null? (cdr parsed-expr)) '()) (else (remove-applic-lambda-nil (cdr parsed-expr))))))
				 (cons left right)
				
			    ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; seif 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define check-bp
  (lambda (var scope)
    (if (null? scope)
	#f
	(let ((mem (member var (car scope))))
	  (if mem
	      #t
	      (check-bp var (cdr scope)))))))


(define get-list-last
  (lambda (lst)
    (if (null? (cdr lst))
	(car lst)
	(get-list-last (cdr lst)))))

(define remove-list-last
  (lambda (ret lst)
    (if (null? (cdr lst))
	ret
	(remove-list-last (append ret (list (car lst))) (cdr lst)))))

#;(define dfs (lambda (tree)
	      (letrec ((n 0)
		       (stack '())
		       (run (lambda (t)
			      (cond ((null? t) (push t))
				    ((not (pair? t)) #f)
				    (else (run (car t)) (run (cdr t)) (push t) stack))))
		       (push (lambda (exp)
			       (if (member exp stack)
				   #f
				   (begin (set! stack (cons exp stack)) stack))))
			       )
		(filter (lambda (e) (pair? e)) (reverse (run tree)))
		)))


(define replace-box-gs
  (lambda (expr var)
    (cond ((null? expr) expr)
	  ((not (pair? expr)) expr)
	  ((eq? (car expr) 'set) (if (eq? (cadadr expr) var)
				     `(box-set (var ,var) ,(replace-box-gs (caddr expr) var))
				     `(set (var ,var) ,(replace-box-gs (caddr expr) var))
				     ))
	  ;;;;
	  ((eq? (car expr) 'box-set) expr)
	  ((eq? (car expr) 'box-get) expr)
	  ((and (eq? (car expr) 'var) (eq? (cadr expr) var)) `(box-get (var ,var)))
	  ((lambda-form? (car expr)) (if (rebound var expr)
					 expr
					 (map (lambda (exp) (replace-box-gs exp var)) expr)))
	  (else (map (lambda (exp)
		       (replace-box-gs exp var))
		     expr)))))

(define box-set2
  (lambda (expr)
    (cond ((not (pair? expr)) expr)
	  ((lambda-form? (car expr))
	   (let* ((tag (car expr))
		  (params (get-params expr))
		  (boxes (map (lambda (var) `(set (var ,var) (box (var ,var)))) params))
		  (body (cond
			 ((eq? tag 'lambda-simple)
			  (fold-left (lambda (ret next-var) (replace-box-gs ret next-var))
				      (caddr expr)
				      params
				      ))
			 ((eq? tag 'lambda-var) (fold-left (lambda (ret next-var) (replace-box-gs ret next-var))
				      (caddr expr)
				      params
				      ))
			 ((eq? tag 'lambda-opt) (fold-left (lambda (ret next-var) (replace-box-gs ret next-var))
				      (cadddr expr)
				      params
				      ))
			 ))
		  (body (if (eq? (car body) 'seq)
			    (cadr body)
			    body))
		  )
	     (cond ((eq? tag 'lambda-simple) `(lambda-simple ,params (seq ,(append boxes (box-set body)))))
		   ((eq? tag 'lambda-var) `(lambda-var ,@params (seq ,(append boxes (box-set body)))))
		   ((eq? tag 'lambda-opt) `(lambda-opt ,(remove-list-last '() params) ,(get-list-last params) (seq ,(append boxes (box-set body))))))))
	  (else (map box-set expr)))))

(define bound?
  (lambda (var expr)
    (cond ((null? expr) #f)
	  ((not (pair? expr)) #f)
	  ((lambda-form? (car expr)) (or (member var (get-params expr))
					 (bound? var (if (eq? (car expr) 'lambda-opt)
							 (cadddr expr)
							 (caddr expr)))))
	  (else (ormap (lambda (exp) (bound? var exp)) expr)))))

(define set?
  (lambda (var expr)
    (cond ((null? expr) #f)
	  ((not (pair? expr)) #f)
	  ((eq? (car expr) 'set) (eq? (cadadr expr) var))
	  (else (ormap (lambda (exp) (set? var exp)) expr)))))

(define get?
  (lambda (var expr)
    (cond ((null? expr) #f)
	  ((not (pair? expr)) #f)
	  ((and (or (eq? (car expr) 'set) (eq? (car expr) 'box-set)) (or (eq? (cadadr expr) var) (eq? (cadadr expr) 'box-get))) (get? var (cddr expr)))
	  ((eq? (car expr) 'var) (eq? (cadr expr) var))
	  (else (ormap (lambda (exp) (get? var exp)) expr)))))

(define member?
    (lambda (x ls)
      (and (member x ls) #t)))

(define appears-in-lambda
  (lambda (var expr)
    (cond ((null? expr) #f)
	  ((not (pair? expr)) #f)
	  ((lambda-form? (car expr))
	   (let ((body (if (eq? (car expr) 'lambda-opt) (cadddr expr) (caddr expr))))
	     #;(if (eq? (car body) 'var)
		 (eq? (cadr body) var)
		 (ormap (lambda (exp)
			  (or (and (pair? exp) (eq? (car exp) 'var) (eq? (cadr exp) var))
			      (appears-in-lambda var exp)))
	     body))
	     #;(ormap (lambda (x) (if (list? x) (or (member? `(var ,var) x)) (appears-in-lambda var x)) (eq? var x)) body)
	     (cond ((member var (get-params expr)) #f)
		   ((eq? (car body) 'var) (eq? (cadr body) var))
		   (ormap (lambda (exp) (appears-in-lambda var exp)) body))
	     ))
	  (else (ormap (lambda (exp) (appears-in-lambda var exp)) expr)))))

(define rebound
  (lambda (var expr)
    (cond ((null? expr) #f)
	  ((not (pair? expr)) #f)
	  ((lambda-form? (car expr))
	   #;(if (member var (get-params expr))
	       #t
	   (rebound var (if (eq? (car expr) 'lambda-opt) (cadddr expr) (caddr expr))))
	   (member var (get-params expr)))
	  (else (ormap (lambda (exp) (rebound var exp)) expr)))))

    
(define is-bad-var
  (lambda (var expr)
    (let ((cond-bound (bound? var expr))
	  (cond-set (set? var expr))
	  (cond-get (get? var expr))
	  )
      (and cond-bound cond-set cond-get (ormap (lambda (exp) (appears-in-lambda var exp)) expr)))
    ;var
    ))

(define box-set
  (lambda (expr)
    (cond ((not (pair? expr)) expr)
	  ((not (list? expr)) expr)
	  ((lambda-form? (car expr))
	   (let* ((tag (car expr))
		  (params (get-params expr))
		  (bad-params (filter (lambda (var) (is-bad-var var expr)) params))
		  (boxes (fold-left (lambda (ret var)
				      (if (is-bad-var var expr)
					  (append ret (list `(set (var ,var) (box (var ,var)))))
					  ret)) '() bad-params))
		  (body (cond
			 ((eq? tag 'lambda-simple)
			  (fold-left (lambda (ret next-var) (replace-box-gs ret next-var))
				      (caddr expr)
				      bad-params
				      ))
			 ((eq? tag 'lambda-var) (fold-left (lambda (ret next-var) (replace-box-gs ret next-var))
				      (caddr expr)
				      bad-params
				      ))
			 ((eq? tag 'lambda-opt) (fold-left (lambda (ret next-var) (replace-box-gs ret next-var))
				      (cadddr expr)
				      bad-params
				      ))
			 ))
		  (body (if (null? boxes)
			    body
			    (if (eq? (car body) 'seq)
			    (cdr body)
			    body)))
		  )
	     (if (null? boxes)
		 (cond ((eq? tag 'lambda-simple) `(lambda-simple ,params ,(box-set body)))
		       ((eq? tag 'lambda-var) `(lambda-var ,@params ,(box-set body)))
		       ((eq? tag 'lambda-opt) `(lambda-opt ,(remove-list-last '() params) ,(get-list-last params) ,(box-set body))))
		 (cond ((eq? tag 'lambda-simple) `(lambda-simple ,params (seq ,(append boxes (if (list? (car body)) (car (box-set body)) (list (box-set body)))))))
		       ((eq? tag 'lambda-var) `(lambda-var ,@params (seq ,(append boxes) (if (list? (car body)) (car (box-set body)) (list (box-set body))))))
		       ((eq? tag 'lambda-opt) `(lambda-opt ,(remove-list-last '() params) ,(get-list-last params) (seq ,(append boxes (if (list? (car body)) (car (box-set body)) (list (box-set body)))))))))))
	   (else (map box-set expr)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;; seif 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lambda-form?
  (lambda (exp)
    (member exp '(lambda-simple lambda-var lambda-opt))))

(define get-params
  (lambda (lambda-exp)
    (cond ((eq? (car lambda-exp) 'lambda-simple)
	   (cadr lambda-exp))
	   ((eq? (car lambda-exp) 'lambda-opt)
	    (append (cadr lambda-exp) (list (caddr lambda-exp))))
	   ((eq? (car lambda-exp) 'lambda-var)
	    (list (cadr lambda-exp)))
	   (else (format "Illegal expression detected, deleting system 32 now"))
	   )))

(define find-nesting
  (lambda (var scope n)
    (if (null? scope)
	'(-1 -1)
	(let ((mem (member var (car scope))))
	  (if mem
	      (list n (- (length (car scope)) (length mem)))
	      (find-nesting var (cdr scope) (+ n 1)))))))

(define pe->lex-pe
  (lambda (expr)
    (letrec ((run (lambda (expr scope params)
		    (if (not (pair? expr))
			expr
			(cond ((not (list? expr)) expr)
			      ((lambda-form? (car expr)) (formatted expr (cons params scope) (get-params expr)))
			      ((eq? (car expr) 'var) (let ((mem (member (cadr expr) params)))
						       (if mem
							   `(pvar ,(cadr expr) ,(- (length params) (length mem)))
							   (let ((ans (find-nesting (cadr expr) scope 0)))
							     (if (< -1 (car ans))
								 `(bvar ,(cadr expr) ,(car ans) ,(cadr ans))
								 `(fvar ,(cadr expr)))))))
			      (else (map (lambda (exp)
					   (run exp scope params))
					 expr)
				    )))))
	     (formatted
	      (lambda (expr scope params)
		(cond ((eq? (car expr) 'lambda-simple) `(lambda-simple ,(get-params expr) ,(run (caddr expr) scope params)))
		      ((eq? (car expr) 'lambda-var) `(lambda-var ,@(get-params expr) ,(run (caddr expr) scope params)))
		      ((eq? (car expr) 'lambda-opt) `(lambda-opt ,(cadr expr) ,(caddr expr) ,(run (cadddr expr) scope params))))
		))
	     )
	     (if (list? (car expr))
		 (map (lambda (exp) (run exp '() '())) expr)
		 (if (lambda-form? (car expr))
		     (formatted expr '() (get-params expr))
		     (if (eq? (car expr) 'var)
			 (run expr '() '())
			 (map (lambda (exp) (run exp '() '())) expr))))
	     )))

(define get-last-of-seq
  (lambda (exp)
    (let ((is-last?
	     (lambda (exp)
	       (if (null? (cdr exp)) #t #f))))
      (or (and (is-last? exp) (car exp)) (get-last-of-seq (cdr exp))))))

(define format-tc2
  (lambda (exp)
    (cond
     ;;;;;;;;;;;
     ((not (pair? exp)) exp)
     ;;;;;;;;;;;
     ((is-lambda? exp) (let ((body (if (eq? (car exp) 'lambda-opt) (cadddr exp) (caddr exp)))
			     (yossi (if (eq? (car exp) 'lambda-opt) cdddr cddr)))
			      (if (eq? (car body) 'applic) (begin (set-car! body 'tc-applic) (map format-tc (cdr body))  #;exp) (begin (set-car! (yossi body) (format-tc body)) exp))))
	  ((eq? (car exp) 'seq) (format-tc (get-last-of-seq (cadr exp))))
	  #;((eq? (car exp) 'if3) (begin
				  (if (eq? (car (caddr exp)) 'applic)
					    (begin (set-car! (caddr exp) 'tc-applic) (map format-tc (cdr (caddr exp))) exp)
					    (format-tc (caddr exp)))
				  (if (eq? (car (cadddr exp)) 'applic)
				      (begin (set-car! (cadddr exp) 'tc-applic) (map format-tc (cdr (cadddr exp))) exp)
	  (begin (set-car! (cadddr exp) (format-tc (cadddr exp))) exp))))
	  ((eq? (car exp) 'if3)
	   (let ((pred (cadr exp))
		 (dit (caddr exp))
		 (dif (cadddr exp)))
	     `(if3 ,pred ,(format-tc dit) ,(format-tc dif))))

	  (else exp))))

(define format-if-tc
  (lambda (exp)
    (if (eq? (car exp) 'applic) `(tc-applic ,@(format-tc (cdr exp))) (format-tc exp))))

(define handle-seq
  (lambda (exp)
    (letrec
	((handle-last
	  (lambda (exp)
	    (if (eq? (caar exp) 'applic) (list `(tc-applic  ,(format-tc (cdar exp)))) exp)))
	 (run
	     (lambda (exp)
	       (format-tc exp)))
	 (run2
	  (lambda (exp)
	    (if (null? (cdr exp)) (handle-last exp) (append (list (car exp)) (run2 (cdr exp)))))))
      (begin (map run (car exp)) (run2 (car exp))))))

(define format-tc
  (lambda (exp)
    (cond ((lambda-form? (car exp)) (let ((body (if (eq? (car exp) 'lambda-opt) (cadddr exp) (caddr exp)))
			     (head (if (eq? (car exp) 'lambda-opt) `(,(car exp) ,(cadr exp) ,(caddr exp)) `(,(car exp) ,(cadr exp)))))
			      (if (eq? (car body) 'applic) (begin (set-car! body 'tc-applic) `(,@head ,(format-tc body)))
				  `(,@head ,(format-tc body)))))
	  #;((eq? (car exp) 'seq) `(seq ,@(map format-tc (cdr exp))))
	  ((eq? (car exp) 'seq) `(seq ,(handle-seq (cdr exp))))
	  ((eq? (car exp) 'if3) (let ((pred (cadr exp))
				      (dit (caddr exp))
				      (dif (cadddr exp)))
				  `(if3 ,(format-if-tc pred) ,(format-if-tc dit) ,(format-if-tc dif))))
	  ((eq? (car exp) 'applic) `(applic ,@(map format-tc (cdr exp))))
	  ((eq? (car exp) 'tc-applic) `(tc-applic ,@(map annotate-tc (cdr exp))))
	  (else exp))))
						       
(define annotate-tc3
  (lambda (exp)
    (letrec ((check-tc
	   (lambda (exp)
	     (cond ((not (pair? exp)) exp)
		   ((and (list? (car exp)) (not (null? exp))) (map check-tc exp))
		   ((is-lambda? exp) (format-tc exp))
		   ((null? exp) exp)
		   (else exp))))
	    #; (if (is-lambda? exp) (format-tc exp) exp))
      (cond ((null? exp) exp)
	    ((or (lambda-form? (car exp)) (and (pair? exp) (eq? (car exp) 'if3))) (check-tc exp))
	    (else (list? exp) (map check-tc exp))
	    ))))


(define annotate-tc
  (lambda (expr)
    (letrec ((loop
	      (lambda (exp tc?)
		(if (not (pair? exp))
		    exp
		    (cond ((tagged-by? exp 'const) exp)
			  ((tagged-by? exp 'fvar) exp)
			  ((tagged-by? exp 'bvar) exp)
			  ((tagged-by? exp 'pvar) exp)
			  ((tagged-by? exp 'or) (letrec ((or-loop
							  (lambda (exp tc?)
							    (if (null? (cdr exp))
								(loop exp tc?)
								(append (loop (car exp) #f) (or-loop (cdr exp) tc?))))))
						  `(or ,@(or-loop (cdr exp) tc?))))
			  ((tagged-by? exp 'if3) `(if3 ,(loop (cadr exp) #f) ,(loop (caddr exp) tc?) ,(loop (cadddr exp) tc?)))
			  ((tagged-by? exp 'def) `(def ,(cadr exp) ,(loop (caddr exp) #f)))
			  ((tagged-by? exp 'lambda-simple) `(lambda-simple ,(cadr exp) ,(loop (caddr exp) #t)))
			  ((tagged-by? exp 'lambda-opt) `(lambda-opt ,(cadr exp) ,(caddr exp) ,(loop (cadddr exp) #t)))
			  ((tagged-by? exp 'lambda-var) `(lambda-var ,(cadr exp) ,(loop (caddr exp) #t)))
			  ((tagged-by? exp 'applic) (let ((proc (cadr exp))
							  (args (caddr exp)))
						      (let ((new-proc (loop proc #f))
							    (new-args (map (lambda (pe) (loop pe #f)) args)))
							(if tc?
							    `(tc-applic ,new-proc ,new-args)
							    `(applic ,new-proc ,new-args)))))
			  ((tagged-by? exp 'box-get) exp)
			  ((tagged-by? exp 'set) `(set ,(cadr exp) ,(loop (caddr exp) tc?)))
			  ((tagged-by? exp 'box-set) `(box-set ,(cadr exp) ,(loop (caddr exp) tc?)))
			  ((tagged-by? exp 'seq) (letrec ((seq-loop
							   (lambda (exp tc?)
							     (if (null? (cdr exp))
								 (list (loop (car exp) tc?))
								 (cons (loop (car exp) #f) (seq-loop (cdr exp) tc?))))))
						   `(seq ,(seq-loop (cadr exp) tc?))))
			  (else exp))))))
      #;(car (map (lambda (x) (loop x #f)) (list expr)))
      (loop expr #f))))
		    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CODE GEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-make-label
  (lambda (prefix)
    (lambda ()
      (let ((n 0))
	(lambda ()
	  (set! n (+ n 1))
	  (string->symbol
	   (string-append prefix (number->string n))))))))

(define make-label-if3-else (make-make-label "L_if3_else_"))
(define make-label-if3-true (make-make-label "L_if3_true_"))
(define make-label-if3-exit (make-make-label "L_if3_exit_"))
(define make-label-or-exit (make-make-label "L_or_exit_"))
(define make-label-clos-body (make-make-label "L_clos_body_"))
(define make-label-clos-exit (make-make-label "L_clos_exit_"))
(define make-label-codegen (make-make-label "L_codegen_"))

(define l-else (make-label-if3-else))
(define l-if3-exit (make-label-if3-exit))
(define l-if3-true (make-label-if3-true))
(define l-or (make-label-or-exit))
(define l-clos-body (make-label-clos-body))
(define l-clos-exit (make-label-clos-exit))
(define l-codegen (make-label-codegen))

(define tagged-by?
  (lambda (pe tag)
    (if (not (pair? pe))
	#f
	(eq? tag (car pe)))))

(define global-lambda-counter -1)

(define look-up-const
  (lambda (sym-name)
    sym-name
    ))

(define handle_if
  (lambda (pe)
    (let ((L-else (l-else))
	  (L-exit (l-if3-exit))
	  (L-true (l-if3-true)))
	(string-append
	 (code-gen (cadr pe))
	 "CMP(INDD(R0,0),IMM(T_BOOL));\n"
	 "JUMP_NE(" (symbol->string L-true) ");\n"
	 "CMP(INDD(R0,1),IMM(0));\n"
	 "JUMP_EQ(" (symbol->string L-else) ");\n"
	 (symbol->string L-true) ":\n"
	 (code-gen (caddr pe))
	 "JUMP(" (symbol->string L-exit) ");\n"
	 (symbol->string L-else) ":\n"
	 (code-gen (cadddr pe))
	 (symbol->string L-exit) ":\n")
	))
  )

(define handle_seq
  (lambda (pe)
    (fold-left (lambda (emit exp) (string-append emit (code-gen exp))) "" (cadr pe))
    ))

(define handle_or
  (lambda (pe)
    (let*
	((l-exit (l-or))
	 (ans "")
	 (sent (string-append
		;;;;;;;;;;;;;;"CMP(R0, IMM(SOB_FALSE));\n"
		"CMP(INDD(R0,0),IMM(T_BOOL));\n"
		"JUMP_NE(" (symbol->string l-exit) ");\n"
		"CMP(INDD(R0,1),IMM(0));\n"
		"JUMP_NE(" (symbol->string l-exit) ");\n")))
      (letrec ((func
		   (lambda (pe)
		     (if (null? (cdr pe))
			 (string-append ans (code-gen (car pe)) (symbol->string l-exit) ":\n")
			 (begin (set! ans (string-append ans (code-gen (car pe)) sent)) (func (cdr pe))))
		     )))
	   (func (cadr pe))))))

(define handle_applic
  (lambda (pe)
    (let ((m (length (caddr pe)))
	  (proc (cadr pe))
	  (emit ""))
      (letrec ((func
		(lambda (exp)
		  (if (null? exp)
		      emit
		      (begin (set! emit (string-append emit
						       (code-gen (car exp))
						       "PUSH(R0);\n"))
			     (func (cdr exp)))))))
	(string-append (func (reverse (caddr pe)))
		       
		       "PUSH(IMM(" (number->string m) "));\n"
		       "SHOW(\" ************* \", R0)"
		       (code-gen proc)

		       "INFO;\n"
		       "SHOW(\"AFTER PUSHING CALCULATING PROCEDURE\", R0);\n"

		       "//INFO;\n"
		       
		       "CMP(INDD(R0,0), IMM(T_CLOSURE));\n"
		       "JUMP_NE(L_error_cannot_apply_non_clos);\n"
		       "PUSH(INDD(R0,1));\n"
		       "CALLA(INDD(R0,2));\n"
		       "DROP(1);\n"
		       "POP(R1);\n"
		       "DROP(R1);\n"
		       )))))

(define handle_tc_applic
  (lambda (pe)
    (let ((m (length (caddr pe)))
	  (proc (cadr pe))
	  (emit ""))
      (letrec ((func
		(lambda (exp)
		  (if (null? exp)
		      emit
		      (begin (set! emit (string-append emit
						       (code-gen (car exp))
						       "PUSH(R0);\n"))
			     (func (cdr exp)))))))
	(string-append (func (reverse (caddr pe)))
		       "PUSH(IMM(" (number->string m) "));\n"
		       (code-gen proc)

		       "//INFO;\n"
		       
		       "CMP(INDD(R0,0),IMM(T_CLOSURE));\n"
		       "JUMP_NE(L_error_cannot_apply_non_clos);\n"
		       "PUSH(INDD(R0,1));\n"
		       "PUSH(FPARG(-1));\n" ; ret address
		       "MOV(R1,FPARG(-2));\n" ; old fp

		       "MOV(R15,FP);\n"
		       "SUB(R15,IMM(3));\n"
		       "SUB(R15,STACK(R15));\n"

		       #;"INFO;\n"

		       #;"SHOW(\"RET ADDR BEFORE TC-APPLIC\", FPARG(-1));\n"
		       #;"SHOW(\"OLD ARG COUNT m\", FPARG(1));\n"

		       "BEGIN_LOCAL_LABELS tc_loop, tc_loop_exit;\n"
		       "MOV(R2,SP);\n"
		       "SUB(R2,STARG(1));\n"
		       "SUB(R2,3);\n"

		       #;"SHOW(\"NEW FRAME\", R2);\n"
		       
		       "MOV(R3,FP);\n"
		       "SUB(R3,FPARG(1));\n"
		       "SUB(R3,4);\n"
		       "MOV(R4,IMM(0));\n"
		       "MOV(R5,SP);\n"
		       "SUB(R5,R2);\n"
		       "tc_loop:\n"
		       "CMP(R4,R5);\n"
		       "JUMP_GE(tc_loop_exit);\n"
		       "MOV(STACK(R3),STACK(R2));\n"
		       "INCR(R2);\n"
		       "INCR(R3);\n"
		       "INCR(R4);\n"
		       "JUMP(tc_loop);\n"
		       "tc_loop_exit:\n"
		       "NOP;\n"
		       "END_LOCAL_LABELS;\n"

		       #;"INFO;\n"
		       
		       "ADD(R15,R4);\n"
		       "MOV(SP,R3);\n"
		       
		       "MOV(FP,R1);\n"

		       
		       #;"SHOW(\"RET ADDR AFTER TC-APPLIC\", STARG(-1));\n"
		       #;"SHOW(\"NEW ARG COUNT m\", STARG(1));\n"
		       #;"SHOW(\"STACK?\", SP);\n"
		       
		       "JUMPA(INDD(R0,2));\n"
		       "NOP;\n"
		       )))))
		       
		       

(define handle_lambda_simple
  (lambda (pe)
    (let ((L-body (l-clos-body))
	  (L-exit (l-clos-exit))
	  (m (length (cadr pe)))
	  (ans 'moshe))
      (set! global-lambda-counter (+ global-lambda-counter 1))
      
      (set! ans (string-append "MOV(R1,FPARG(0));\n"
			       "MOV(R2, IMM(" (number->string global-lambda-counter) "));\n"
			       "INCR(R2);\n"
			       "PUSH(R2);\n"
			       "CALL(MALLOC);\n"
			       "DROP(1);\n"
			       "MOV(R2,R0);\n"
			       "BEGIN_LOCAL_LABELS L_lambda_make_env, L_lambda_env_exit;\n"
			       "MOV(R3, IMM(0));\n"
			       "MOV(R4, IMM(1));\n"
			       "L_lambda_make_env:\n"
			       "CMP(R3, IMM(" (number->string global-lambda-counter)  "));\n"
			       "JUMP_GE(L_lambda_env_exit);\n"
			       "MOV(R15,INDD(R1,R3));\n"
			       "MOV(INDD(R2,R4),R15);\n"
			       "INCR(R3);\n"
			       "INCR(R4);\n"
			       "JUMP(L_lambda_make_env);\n"
			       "L_lambda_env_exit:\n"
			       "NOP;\n"
			       "END_LOCAL_LABELS;\n"
			       "MOV(R3,FPARG(1));\n"
			       "PUSH(R3);\n"
			       "CALL(MALLOC);\n"
			       "DROP(1);\n"
			       "MOV(INDD(R2,0),R0);\n"
			       "MOV(R4,INDD(R2,0));\n"
			       "MOV(R5,0);\n" ;i
			       "MOV(R6,2);\n" ;j
			       "BEGIN_LOCAL_LABELS L_lambda_make_params, L_lambda_params_exit;\n"
			       "L_lambda_make_params:\n"
			       "CMP(R5,R3);\n"
			       "JUMP_GE(L_lambda_params_exit);\n"
			       "MOV(INDD(R4,R5),FPARG(R6));\n"
			       "INCR(R5);\n"
			       "INCR(R6);\n"
			       "JUMP(L_lambda_make_params);\n"
			       "L_lambda_params_exit:\n"
			       "PUSH(IMM(3));\n"
			       "CALL(MALLOC);\n"
			       "DROP(1);\n"
			       "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
			       "MOV(INDD(R0,1),R2);\n"
			       "MOV(INDD(R0,2),LABEL(" (symbol->string L-body) "));\n"
			       "JUMP(" (symbol->string L-exit)  ");\n"
			       "END_LOCAL_LABELS;\n"

			       (symbol->string L-body) ":\n"
			       "PUSH(FP);\n"
			       "MOV(FP,SP);\n"
			       "CMP(FPARG(1),IMM(" (number->string m) "));\n"
			       "JUMP_NE(L_error_lambda_args_count);\n"
			       (code-gen (caddr pe))
			       "POP(FP);\n"
			       "RETURN;\n"
			       (symbol->string L-exit) ":\n"))
      
      (set! global-lambda-counter (- global-lambda-counter 1))
      ans   
      )))

(define handle_lambda_opt
  (lambda (pe)
    (let* ((args-list (cadr pe))
	   (m (length args-list))
	   (L-body (l-clos-body))
	   (L-exit (l-clos-exit))
	   (body (cadddr pe))
	   (ans 'yossi))
      (set! global-lambda-counter (+ global-lambda-counter 1))
      (set! ans (string-append "MOV(R1,FPARG(0));\n"
			       "MOV(R2, IMM(" (number->string global-lambda-counter) "));\n"
			       "INCR(R2);\n"
			       "PUSH(R2);\n"
			       "CALL(MALLOC);\n"
			       "DROP(1);\n"
			       "MOV(R2,R0);\n"
			       "BEGIN_LOCAL_LABELS L_lambda_make_env, L_lambda_env_exit;\n"
			       "MOV(R3, IMM(0));\n"
			       "MOV(R4, IMM(1));\n"
			       "L_lambda_make_env:\n"
			       "CMP(R3, IMM(" (number->string global-lambda-counter)  "));\n"
			       "JUMP_GE(L_lambda_env_exit);\n"
			       "MOV(R15,INDD(R1,R3));\n"
			       "MOV(INDD(R2,R4),R15);\n"
			       "INCR(R3);\n"
			       "INCR(R4);\n"
			       "JUMP(L_lambda_make_env);\n"
			       "L_lambda_env_exit:\n"
			       "NOP;\n"
			       "END_LOCAL_LABELS;\n"
			       "MOV(R3,FPARG(1));\n"
			       "PUSH(R3);\n"
			       "CALL(MALLOC);\n"
			       "DROP(1);\n"
			       "MOV(INDD(R2,0),R0);\n"
			       "MOV(R4,INDD(R2,0));\n"
			       "MOV(R5,0);\n" ;i
			       "MOV(R6,2);\n" ;j
			       "BEGIN_LOCAL_LABELS L_lambda_make_params, L_lambda_params_exit;\n"
			       "L_lambda_make_params:\n"
			       "CMP(R5,R3);\n"
			       "JUMP_GE(L_lambda_params_exit);\n"
			       "MOV(INDD(R4,R5),FPARG(R6));\n"
			       "INCR(R5);\n"
			       "INCR(R6);\n"
			       "JUMP(L_lambda_make_params);\n"
			       "L_lambda_params_exit:\n"
			       "PUSH(IMM(3));\n"
			       "CALL(MALLOC);\n"
			       "DROP(1);\n"
			       "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
			       "MOV(INDD(R0,1),R2);\n"
			       "MOV(INDD(R0,2),LABEL(" (symbol->string L-body) "));\n"
			       "JUMP(" (symbol->string L-exit)  ");\n"
			       "END_LOCAL_LABELS;\n"

			       (symbol->string L-body) ":\n"
			       "PUSH(FP);\n"
			       "MOV(FP,SP);\n"

			       "CALL(MAKE_SOB_NIL);\n"
			       "MOV(R1, R0);\n"
			       "MOV(R2, FPARG(1));\n"
			       "INCR(R2);\n"
			       "MOV(R3, IMM("(number->string m)"));\n"
			       "ADD(R3, IMM(1));\n"
			       "BEGIN_LOCAL_LABELS label_for_loop, label_for_exit, fix_stack_up, stack_fix_end, label_for_loop2, label_for_loop3, label_for_exit2, label_for_exit3;\n"
			       "label_for_loop:\n"
			       "CMP(R2, R3);\n"
			       "JUMP_LE(label_for_exit);\n"
			       "PUSH(R1);\n"
			       "PUSH(FPARG(R2));\n"
			       "PUSH(IMM(2));\n"
			       "PUSH(IMM(0xDEF));\n"
			       "CALL(CONS);\n"
			       "DROP(4);\n"
			       "DECR(R2);\n"
			       "MOV(R1,R0);\n"
			       "JUMP(label_for_loop);\n"
			       "label_for_exit:\n"
			       "PUSH(R1);\n"
			       "CALL(IS_SOB_NIL);\n"
			       "DROP(1);\n"
			       "CMP(R0, IMM(1));\n"
			       "JUMP_EQ(fix_stack_up);\n"

			       "MOV(R6,STARG(2));\n"
			       "MOV(R3,SP);\n"
			       "SUB(R3,IMM(4));\n"
			       "SUB(R3,R6);\n"
			       "MOV(STACK(R3),R1);\n"
			       "MOV(R4,R3);\n"
			       "INCR(R4);\n"
			       "MOV(R5,R4);\n"
			       "ADD(R5,R6);\n"
			       "SUB(R5," (number->string (+ m 1)) ");\n"
			       "label_for_loop3:\n"
			       "CMP(R4,IMM(" (number->string (+ m 4 1)) "));\n"
			       "JUMP_GE(label_for_exit3);\n"
			       "MOV(STACK(R4),STACK(R5));\n"
			       "INCR(R4);\n"
			       "INCR(R5);\n"
			       "JUMP(label_for_loop3);\n"
			       "label_for_exit3:\n"
			       "SUB(R6,"(number->string m)");\n"
			       "DECR(R6);"
			       "SUB(SP,R6);\n"
			       "SUB(FP,R6);\n"
			       "ADD(R6,4);\n"
			       "JUMP(stack_fix_end);\n"
			       
			       "fix_stack_up:\n"
			       "MOV(R3,SP);\n"
			       "SUB(R3,4);\n";
			       "SUB(R3," (number->string m)  ");\n"
			       "MOV(R4,R3);\n"
			       "INCR(R4);\n"
			       "MOV(R10,STACK(R4));\n"
			       "MOV(STACK(R4),STACK(R3));\n"
			       "INCR(R3);\n"
			       "label_for_loop2:\n"
			       "CMP(R3,SP);\n"
			       "JUMP_GE(label_for_exit2);\n"
			       "INCR(R4);\n"
			       "MOV(R2,STACK(R4));\n"
			       "MOV(STACK(R4),R10);\n"
			       "MOV(R10,R2);\n"
			       "INCR(R3);\n"
			       "JUMP(label_for_loop2);\n"
			       "label_for_exit2:\n"
			       "MOV(R15,SP);\n"
			       "SUB(R15,IMM(" (number->string (+ m 4)) "));\n"
			       "MOV(STACK(R15),R1);\n"
			       #;("MOV(STACK(" (number->string (+ m 4)) "), IND(R0));\n") ; STACK with displacement
			       "INCR(SP);\n"
			       "INCR(FP);\n"
			       "stack_fix_end:\n"
			       
			       "MOV(FPARG(1),IMM(" (number->string (+ m 1)) "));\n"
			       "END_LOCAL_LABELS;\n"

			       (code-gen body)
			       "POP(FP);\n"
			       "RETURN;\n"

			       (symbol->string L-exit) ":\n"

			       ))

      (set! global-lambda-counter (- global-lambda-counter 1))
      ans
      )))

(define handle_lambda_var
  (lambda (pe)
    (let ((var (cadr pe))
	  (body (caddr pe)))
      (handle_lambda_opt `(debug-info ,'() ,var ,body)))))

(define handle_pvar_boxget
  (lambda (pe)
    (let* ((var (cadr pe))
	   (min (caddr var)))
      (string-append "MOV(R1,IMM(2));\n"
		     "ADD(R1,IMM(" (number->string min) "));\n"
		     "MOV(R0,FPARG(R1));\n"
		     "MOV(R0,IND(R0));\n")
      )))

(define handle_pvar_set
  (lambda (pe)
    (let* ((var (cadr pe))
	   (min (caddr var))
	   (exp (caddr pe)))
      (string-append (code-gen exp)
		     "MOV(R1,IMM(2));\n"
		     "ADD(R1,IMM(" (number->string min) "));\n"
		     "MOV(FPARG(R1),R0);\n"
		     "CALL(MAKE_SOB_VOID);\n"))))

(define handle_pvar_boxset
  (lambda (pe)
    (let* ((var (cadr pe))
	   (min (caddr var))
	   (exp (caddr pe)))
      (string-append (code-gen exp)
		     "MOV(R1,IMM(2));\n"
		     "ADD(R1,IMM(" (number->string min) "));\n"
		     "MOV(R1,FPARG(R1));\n"
		     "MOV(IND(R1),R0);\n"
		     "CALL(MAKE_SOB_VOID);\n"))))

(define handle_pvar_get
  (lambda (pe)
    (let ((min (caddr pe)))
      (string-append "MOV(R0,FPARG(" (number->string (+ 2 min)) "));\n"))))

(define handle_bvar_get
  (lambda (pe)
    (let ((maj (caddr pe))
	  (min (cadddr pe)))
      (string-append "MOV(R0,FPARG(0));\n"
		     "MOV(R0,INDD(R0," (number->string maj) "));\n"
		     "MOV(R0,INDD(R0," (number->string min)  "));\n"))))

(define handle_bvar_set
  (lambda (pe)
    (let* ((var (cadr pe))
	   (maj (caddr var))
	   (min (cadddr var))
	   (exp (caddr pe)))
      (string-append (code-gen exp)
		     "MOV(R1,R0);\n"
		     "MOV(R0,FPARG(0));\n"
		     "MOV(R0,INDD(R0," (number->string maj)  "));\n"
		     "MOV(INDD(R0," (number->string min) "), R1);\n"
		     "CALL(MAKE_SOB_VOID);\n"))))

(define handle_bvar_boxget
  (lambda (pe)
    (let* ((var (cadr pe)))
      (string-append (handle_bvar_get var) ;re-using code generation
		     "MOV(R0,IND(R0));\n"))))

(define handle_bvar_boxset
  (lambda (pe)
    (let* ((var (cadr pe))
	   (exp (caddr pe)))
      (string-append (code-gen exp)
		     "MOV(R1,R0);\n"
		     (handle_bvar_get var) ;re-using code generation
		     "MOV(IND(R0),R1);\n"
		     "CALL(MAKE_SOB_VOID);\n"))))

(define handle_fvar_get
  (lambda (pe)
    (let ((fvar_addr (lookup-var free-var-table pe)))
      (if (= fvar_addr -1)
	  (format "error: fvar ~s not defined" (cadr pe))
	  (string-append "MOV(R0,IND(" (number->string fvar_addr) "));\n")))))

(define handle_fvar_set
  (lambda (pe)
    (let* ((fvar (cadr pe))
	   (fvar_addr (lookup-var free-var-table fvar))
	   (exp (caddr pe)))
      (if (= fvar_addr -1)
	  (format "error: fvar ~s not defined" (cadr fvar))
	  (string-append (code-gen exp)
			 "MOV(IND(" (number->string fvar_addr) "),R0);\n"
			 "CALL(MAKE_SOB_VOID);\n")))))

(define handle_define
  (lambda (pe)
    (let* ((fvar (cadr pe))
	   (fvar_addr (lookup-var free-var-table fvar))
	   (exp (caddr pe)))
      (if (= fvar_addr -1)
	  (format "error: fvar ~s not defined" (cadr fvar))
	  (string-append (code-gen exp)
			 "MOV(IND(" (number->string fvar_addr) "),R0);\n"
			 "CALL(MAKE_SOB_VOID);\n")))))

(define handle_box
  (lambda (pe)
    (let* ((var (cadr pe)))
      (cond ((tagged-by? var 'pvar)
	     (let ((min (caddr var)))
	       (string-append "MOV(R1,FPARG(" (number->string (+ 2 min)) "));\n"
			      "PUSH(IMM(1));\n"
			      "CALL(MALLOC);\n"
			      "DROP(1);\n"
			      "MOV(IND(R0),R1);\n")))
	    ((tagged-by? var 'bvar)
	     (let ((maj (caddr var))
		   (min (cadddr var)))
	       (string-append "MOV(R1,FPARG(0));\n"
			      "MOV(R1,INDD(R1," (number->string maj) "));\n"
			      "MOV(R1,INDD(R1," (number->string min)  "));\n"
			      "PUSH(IMM(1));\n"
			      "CALL(MALLOC);\n"
			      "DROP(1);\n"
			      "MOV(IND(R0),R1);\n")))))))
			 

(define memory-start-address 10)
(define memory-start memory-start-address)
(define void-addr memory-start-address)
(define null-addr (+ memory-start-address 1))
(define bool-false-addr (+ memory-start-address 2))
(define bool-true-addr (+ memory-start-address 4))
(define symbol-table-pointer (- memory-start-address 5))

(define re-define (lambda () (set! memory-start memory-start-address)
			  (set! global-const-table '())
			  (set! free-var-table '())))


(define make-fvar-table
  (lambda (pe)
    (let ((n memory-start)
	  (fvar-table '()))
      (let ((vars (topo-var pe)))
	(begin (set! fvar-table (append fvar-table (list (list 'cons n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'car n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'cdr n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'null? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'set-cdr! n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'set-car! n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'not n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'pair? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'bin+ n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'bin- n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'apply n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'boolean? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'char? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'integer? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'procedure? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'rational? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'string? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'symbol? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'vector? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'zero? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'char->integer n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'integer->char n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'denominator n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'numerator n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'string-length n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'string-ref n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'string-set! n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'vector-length n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'vector-ref n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'vector-set! n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'make-string n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'make-vector n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'vector n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list '< n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list '> n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list '= n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list '* n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list '/ n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'eq? n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'symbol->string n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'string->symbol n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'remainder n)))) (set! n (+ n 1)))
	(begin (set! fvar-table (append fvar-table (list (list 'list n)))) (set! n (+ n 1)))
	(fold-left (lambda (_a next-var) (set! fvar-table (append fvar-table  (list (list (cadr next-var) n)))) (set! n (+ n 1))) '() vars)
	(set! memory-start n)
	fvar-table))))

(define lookup-var
  (lambda (table var)
    (letrec ((loop (lambda (rest)
		     (cond ((null? rest) -1)
			   (else (let ((entry (car rest)))
				   (if (eq? (cadr var) (car entry))
				       (cadr entry)
				       (loop (cdr rest)))))))))
      (loop table))))

(define look-up-const
  (lambda (table c)
    (letrec ((run (lambda (rest)
		    (if (null? rest) -1
			(let ((next-entry (car rest)))
			  (cond ((void? c) void-addr)
				((null? c) null-addr)
				((boolean? c) (if (eq? #f c)
						  bool-false-addr
						  bool-true-addr))
				((number? c) (if (and (number? (cadr next-entry)) (= c (cadr next-entry))) (car next-entry) (run (cdr rest))))
				((char? c) (if (and (char? (cadr next-entry)) (char=? c (cadr next-entry))) (car next-entry) (run (cdr rest))))
				((string? c) (if (and (string? (cadr next-entry)) (string=? c (cadr next-entry))) (car next-entry) (run (cdr rest))))
				((symbol? c) (if (and (symbol? (cadr next-entry)) (symbol=? c (cadr next-entry))) (car next-entry) (run (cdr rest))))
				((vector? c) (if (equal? c (cadr next-entry)) (car next-entry) (run (cdr rest))))
				((pair? c) (if (and (pair? (cadr next-entry)) (equal? c (cadr next-entry))) (car next-entry) (run (cdr rest))))))))))
      (run table))))

(define make-const-table
    (let ((n memory-start)
	  (const-table '()))
      (lambda (pe)
	(let ((consts (topo pe)))
	  (letrec ((make-const (lambda (c)
				 (let ((c-val (cadr c))
				       (table-entry 'joseph))
				   (cond ((null? c-val) '(dummy const))
					 ((integer? c-val) (set! table-entry (list n c-val (list 945311 c-val))) (set! n (+ n 2)) table-entry)
					 ;;;;;;;;;;;;;;;;;;
					 ((and (rational? c-val) (not (integer? c-val)))
					  (let* ((numer (numerator c-val))
						 (denomi (denominator c-val))
						 (numer-entry (look-up-const const-table numer))
						 (denomi-entry (look-up-const const-table denomi)))
					    (cond ((and (< -1 numer-entry) (< -1 denomi-entry)) (set! table-entry (list n c-val (list 451794 numer-entry denomi-entry))) table-entry)
						  ((and (< -1 numer-entry) (= -1 denomi-entry))
						   (set! const-table (append const-table (list (make-const `(const ,denomi)))))
						   (set! table-entry (list n c-val (list 451794 numer-entry (look-up-const const-table denomi))))
						   (set! n (+ n 3))
						   table-entry)
						  ((and (= -1 numer-entry) (< -1 denomi-entry))
						   (set! const-table (append const-table (list (make-const `(const ,numer)))))
						   (set! table-entry (list n c-val (list 451794 (look-up-const const-table numer) denomi-entry)))
						   (set! n (+ n 3))
						   table-entry)
						  (else
						   (set! const-table (append const-table (list (make-const `(const ,numer)))))
						   (set! const-table (append const-table (list (make-const `(const ,denomi)))))
						   (set! table-entry (list n c-val (list 451794 (look-up-const const-table numer) (look-up-const const-table denomi))))
						   (set! n (+ n 3))
						   table-entry))))
					 ;;;;;;;;;;;;;;;;;;
					 ((string? c-val) (let ((s-len (string-length c-val))
								(s-chars (string->list c-val)))
							    (set! table-entry (list n c-val (append (list 799345 s-len) (map char->integer s-chars)))) (set! n (+ n 2 s-len)) table-entry))
					 ((char? c-val) (set! table-entry (list n c-val (list 181048 (char->integer c-val)))) (set! n (+ n 2)) table-entry)
					 ((symbol? c-val) (let* ((rep-string (symbol->string c-val))
								 (rep-string-entry (look-up-const const-table rep-string)))
							    (if (= rep-string-entry -1)
								(begin (set! const-table (append const-table (list (make-const `(const ,rep-string)))))
								       (set! table-entry (list n c-val (list 368031 (look-up-const const-table rep-string))))
								       (set! n (+ n 2))
								       table-entry)
								(begin (set! table-entry (list n c-val (list 368031 rep-string-entry)))
								       (set! n (+ n 2))
								       table-entry))))
					 ((vector? c-val) (let* ((v-len (vector-length c-val))
								 (v-vals (vector->list c-val))
								 (v-vals-consts (map (lambda (v-member)
										       (let ((v-mem-entry (look-up-const const-table  v-member)))
											 (if (= -1 v-mem-entry)
											     (begin (set! const-table (append const-table (list (make-const `(const ,v-member)))))
												    (look-up-const const-table v-member))
											     v-mem-entry)))
										     v-vals)))
							    (set! table-entry (list n c-val `(,335728 ,v-len ,@v-vals-consts)))
							    (set! n (+ n 2 v-len))
							    table-entry))
					 ((pair? c-val) (let* ((kar (car c-val))
							       (kdr (cdr c-val))
							       (kar-entry (look-up-const const-table kar))
							       (kdr-entry (look-up-const const-table kdr)))
							  (cond ((and (< -1 kar-entry) (< -1 kdr-entry)) (set! table-entry (list n c-val (list 885397 kar-entry kdr-entry))) (set! n (+ n 3)) table-entry)
								((and (< -1 kar-entry) (= -1 kdr-entry))
								 (set! const-table (append const-table (list (make-const `(const ,kdr)))))
								 (set! table-entry (list n c-val (list 885397 kar-entry (look-up-const const-table kdr)))) (set! n (+ n 3)) table-entry)
								((and (= -1 kar-entry) (< -1 kdr-entry))
								 (set! const-table (append const-table (list (make-const `(const ,kar)))))
								 (set! table-entry (list n c-val (list 885397 (look-up-const const-table kar) kdr-entry))) (set! n (+ n 3)) table-entry)
								(else
								 (set! const-table (append const-table (list (make-const `(const ,kar)))))
								 (set! const-table (append const-table (list (make-const `(const ,kdr)))))
								 (set! table-entry (list n c-val (list 885397 (look-up-const const-table kar) (look-up-const const-table kdr)))) (set! n (+ n 3)) table-entry))))
					 (else '(dummy const)))))))
	    (set! const-table (list (list void-addr dummy-void (list 937610))))
	    (set! const-table (append const-table (list (list null-addr '() (list 722689)))))
	    (set! const-table (append const-table (list (list bool-false-addr #f (list 741553 0)))))
	    (set! const-table (append const-table (list (list bool-true-addr #t (list 741553 1)))))
	    (set! n (+ memory-start 6))
	    (for-each (lambda (next-const) (set! const-table (append const-table (list (make-const next-const))))) consts)
	    (set! memory-start n)
	    (filter (lambda (x) (and (pair? x) (not (tagged-by? x 'dummy))) )  const-table))))))
							     
(define global-const-table '())
(define free-var-table '())

(define primitive-cons
  (lambda ()
    (string-append "FVAR_CONS:\n"
		   
		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(9104));\n"
		   "MOV(INDD(R0,2),LABEL(CONS));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar cons)))  "),R0);\n"
		   "JUMP(CONS_exit);\n"
		   
		   "CONS:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   
		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_PAIR));\n"
		   "MOV(INDD(R0,1),FPARG(2));\n"
		   "MOV(INDD(R0,2),FPARG(3));\n"
		   
		   "POP(FP);\n"
		   "RETURN;\n"
		   "CONS_exit:\n"
		   )))

(define primitive-car
  (lambda ()
    (string-append "FVAR_CAR:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(4853));\n"
		   "MOV(INDD(R0,2),LABEL(CAR));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar car)))  "),R0);\n"
		   "JUMP(CAR_exit);\n"

		   "CAR:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "CMP(INDD(R1,0),IMM(T_PAIR));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R0,INDD(R1,1));\n"
		   "POP(FP);\n"
		   "RETURN;\n"
		   "CAR_exit:\n"
		   )))

(define primitive-cdr
  (lambda ()
    (string-append "FVAR_CDR:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(1337));\n"
		   "MOV(INDD(R0,2),LABEL(CDR));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar cdr))) "), R0);\n"
		   "JUMP(CDR_exit);\n"

		   "CDR:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "CMP(INDD(R1,0),IMM(T_PAIR));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R0,INDD(R1,2));\n"
		   "POP(FP);\n"
		   "RETURN;\n"
		   "CDR_exit:\n"
		   )))

(define primitive-null?
  (lambda ()
    (string-append "FVAR_NULLPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(80085));\n"
		   "MOV(INDD(R0,2),LABEL(NULLPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar null?))) "), R0);\n"
		   "JUMP(NULLPRED_exit);\n"

		   "NULLPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "PUSH(FPARG(2));\n"
		   "CALL(IS_SOB_NIL);\n"
		   "DROP(1);\n"
		   
		   "CMP(R0,1);\n"
		   "JUMP_NE(L_nullpred_not_null);\n"
		   "MOV(R0,IMM(" (number->string (look-up-const global-const-table #t))  "));\n"
		   "JUMP(L_nullpred_exit);\n"
		   "L_nullpred_not_null:\n"
		   "MOV(R0,IMM(" (number->string (look-up-const global-const-table #f))  "));\n"

		   "L_nullpred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"
		   "NULLPRED_exit:\n"
		   )))

(define primitive-set-cdr!
  (lambda ()
    (string-append "FVAR_SET_CDR_BANG:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(7714));\n"
		   "MOV(INDD(R0,2),LABEL(SET_CDR_BANG));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar set-cdr!))) "),R0);\n"
		   "JUMP(SET_CDR_BANG_exit);\n"

		   "SET_CDR_BANG:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "MOV(INDD(R1,2),FPARG(3));\n"
		   "MOV(R0,IMM(" (number->string void-addr) "));\n"

		   "POP(FP);\n"
		   "RETURN;\n"
		   "SET_CDR_BANG_exit:\n"
		   )))

(define primitive-set-car!
  (lambda ()
    (string-append "FVAR_SET_CAR_BANG:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(7715));\n"
		   "MOV(INDD(R0,2),LABEL(SET_CAR_BANG));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar set-car!))) "),R0);\n"
		   "JUMP(SET_CAR_BANG_exit);\n"

		   "SET_CAR_BANG:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "MOV(INDD(R1,1),FPARG(3));\n"
		   "MOV(R0,IMM(" (number->string void-addr) "));\n"

		   "POP(FP);\n"
		   "RETURN;\n"
		   "SET_CAR_BANG_exit:\n"
		   )))

(define primitive-not
  (lambda ()
    (string-append "FVAR_NOT:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(20021991));\n"
		   "MOV(INDD(R0,2),LABEL(FNOT));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar not)))  "),R0);\n"
		   "JUMP(FNOT_exit);\n"

		   "FNOT:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_BOOL);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(0));\n"
		   "JUMP_EQ(L_not_make_false);\n"
		   "CMP(INDD(R1,1),IMM(1));\n"
		   "JUMP_EQ(L_not_make_false);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr)  "));\n"
		   "JUMP(L_not_exit);\n"
		   "L_not_make_false:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr)  "));\n"
		   "L_not_exit:\n"

		   "POP(FP);\n"
		   "RETURN;\n"
		   "FNOT_exit:\n")))

(define primitive-pair?
  (lambda ()
    (string-append "FVAR_PAIRPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(17011992));\n"
		   "MOV(INDD(R0,2),LABEL(PAIRPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar pair?)))  "),R0);\n"
		   "JUMP(PAIRPRED_exit);\n"

		   "PAIRPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "PUSH(FPARG(2));\n"
		   "CALL(IS_SOB_PAIR);\n"
		   "DROP(1);\n"
		   "CMP(R0,1);\n"
		   "JUMP_NE(L_pairpred_not_pair);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr)  "));\n"
		   "JUMP(L_pairpred_exit);\n"
		   "L_pairpred_not_pair:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr)  "));\n"
		   
		   "L_pairpred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"
		   "PAIRPRED_exit:\n")))

(define primitive-+
  (lambda ()
    (string-append "FVAR_ADD:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(180052));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_ADD));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar bin+))) "),R0);\n"
		   "JUMP(PRIM_ADD_exit);\n"

		   "PRIM_ADD:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"

		   "PUSH(R1);\n"
		   "PUSH(R2);\n"
		   "PUSH(R7);\n"
		   "PUSH(R8);\n"
		   "PUSH(R5);\n"
		   "PUSH(R6);\n"
		   
		   "SHOW(\"@@@@@@@@@@@@@@@@@@\",R0);\n"
		   "INFO;\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"

		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(0));\n"
		   "JUMP_EQ(L_add_first_frac);\n"
		   "MOV(R2,FPARG(3));\n"
		   "PUSH(R2);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(0));\n"
		   "JUMP_EQ(L_add_mix);\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "ADD(R1,R2);\n"
		   "PUSH(R1);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "JUMP(L_add_finish);\n"
		   "L_add_first_frac:\n"
		   "MOV(R2,FPARG(3));\n"
		   "PUSH(R2);\n"
		   "CALL(IS_SOB_FRACTION);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(0));\n"
		   "BEGIN_LOCAL_LABELS xchange, continue_two_frac;\n"
		   "JUMP_EQ(xchange);\n"
		   "continue_two_frac:\n"
		   "MOV(R7,INDD(R1,2));\n"
		   "MOV(R7,INDD(R7,1));\n"
		   "PUSH(R7);\n"
		   "MOV(R8,INDD(R2,2));\n"
		   "MOV(R8,INDD(R8,1));\n"
		   "PUSH(R8);\n"
		   "CALL(GCD);\n"
		   "DROP(2);\n"
		   "MOV(R4,R0);\n"
		   "MOV(R5,R7);\n"
		   "MUL(R5,R8);\n"
		   "DIV(R5,R4);\n"
		   "MOV(R4,R5);\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "DIV(R5,R7);\n"
		   "MUL(R5,R1);\n"
		   "MOV(R6,R4);\n"
		   "DIV(R6,R8);\n"
		   "MUL(R6,R2);\n"
		   "ADD(R5,R6);\n"


		   
		   "PUSH(R5);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "MOV(R5,R0);\n"
		   "PUSH(R4);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "MOV(R4,R0);\n"
		   "PUSH(R4);\n"
		   "PUSH(R5);\n"
		   "CALL(MAKE_SOB_FRACTION);\n"
		   "DROP(2);\n"
		   "PUSH(R0);\n"
		   "CALL(SIMPLIFY_FRAC);\n"
		   "DROP(1);\n"
		   "JUMP(L_add_finish);\n"
		   "xchange:\n"
		   "NOP;\n"
		   
		   "MOV(R3,R1);\n"
		   "MOV(R1,R2);\n"
		   "MOV(R2,R3);\n"
		   "L_add_mix:\n"
		   "PUSH(IMM(1));\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "PUSH(R0);\n"
		   "PUSH(R1);\n"
		   "CALL(MAKE_SOB_FRACTION);\n"
		   "DROP(2);\n"
		   "MOV(R1,R0);\n"
		   "JUMP(continue_two_frac);\n"
		   "END_LOCAL_LABELS;"
		   "NOP;\n"
		   "L_add_finish:\n"

		   "POP(R6);\n"
		   "POP(R5);\n"
		   "POP(R8);\n"
		   "POP(R7);\n"
		   "POP(R2);\n"
		   "POP(R1);\n"
		   
		   "POP(FP);\n"
		   "RETURN;\n"
		   
		   "PRIM_ADD_exit:\n")))

(define primitive--
  (lambda ()
    (string-append "FVAR_SUB:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(5821000));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_SUB));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar bin-))) "),R0);\n"
		   "JUMP(PRIM_SUB_exit);\n"

		   "PRIM_SUB:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"

		   "PUSH(R1);\n"
		   "PUSH(R2);\n"
		   "PUSH(R7);\n"
		   "PUSH(R8);\n"
		   "PUSH(R5);\n"
		   "PUSH(R6);\n"

		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"

		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(0));\n"
		   "JUMP_EQ(L_sub_first_frac);\n"
		   "MOV(R2,FPARG(3));\n"
		   "PUSH(R2);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(0));\n"
		   "JUMP_EQ(L_sub_mix);\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "SUB(R1,R2);\n"
		   "PUSH(R1);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "JUMP(L_sub_finish);\n"
		   "L_sub_first_frac:\n"
		   "MOV(R2,FPARG(3));\n"
		   "PUSH(R2);\n"
		   "CALL(IS_SOB_FRACTION);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(0));\n"
		   "BEGIN_LOCAL_LABELS xchange, continue_two_frac;\n"
		   "JUMP_EQ(xchange);\n"
		   "continue_two_frac:\n"
		   "SHOW(\"inside sub before gcd first num in \",R1);\n"
		   "SHOW(\"inside sub before gcd first num in \",R2);\n"
		   "MOV(R7,INDD(R1,2));\n"
		   "MOV(R7,INDD(R7,1));\n"
		   "PUSH(R7);\n"
		   "MOV(R8,INDD(R2,2));\n"
		   "MOV(R8,INDD(R8,1));\n"
		   "PUSH(R8);\n"
		   "SHOW(\"inside sub before gcd call \",R0);\n"
		   "INFO;\n"
		   "CALL(GCD);\n"
		   "DROP(2);\n"
		   "MOV(R4,R0);\n"
		   "MOV(R5,R7);\n"
		   "MUL(R5,R8);\n"
		   "DIV(R5,R4);\n"
		   "MOV(R4,R5);\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "DIV(R5,R7);\n"
		   "MUL(R5,R1);\n"
		   "MOV(R6,R4);\n"
		   "DIV(R6,R8);\n"
		   "MUL(R6,R2);\n"
		   "SUB(R5,R6);\n"

		   "SHOW(\"IN SUB NUMERATOR\", R5);\n"
		   "SHOW(\"IN SUB DENOMINATOR\", R4);\n"
		   
		   "PUSH(R5);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "MOV(R5,R0);\n"
		   "PUSH(R4);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "MOV(R4,R0);\n"
		   "PUSH(R4);\n"
		   "PUSH(R5);\n"
		   "CALL(MAKE_SOB_FRACTION);\n"
		   "DROP(2);\n"
		   "PUSH(R0);\n"
		   "CALL(SIMPLIFY_FRAC);\n"
		   "DROP(1);\n"
		   "JUMP(L_sub_finish);\n"
		   "xchange:\n"
		   "MOV(R8,INDD(R1,1));\n"
		   "SHOW(\"r8 is \",R8);\n"
		   "MOV(R8,INDD(R8,1));\n"
		   "SHOW(\"r8 is \",R8);\n"
		   "MUL(R8,IMM(-1));\n"
		   "PUSH(R8);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   
		   "PUSH(INDD(R1,2));\n"
		   "PUSH(R0);\n"
		   "CALL(MAKE_SOB_FRACTION);\n"
		   "DROP(2);\n"
		   "MOV(R1,R0);\n"
		   "MOV(R9,INDD(R2,1));\n"
		   "MUL(R9,IMM(-1));\n"

		   "PUSH(R9);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "MOV(R2,R0);\n"
		   "INFO;\n"
		   "MOV(R3,R1);\n"
		   "MOV(R1,R2);\n"
		   "MOV(R2,R3);\n"
		   "L_sub_mix:\n"
		   "PUSH(IMM(1));\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "PUSH(R0);\n"
		   "PUSH(R1);\n"
		   "CALL(MAKE_SOB_FRACTION);\n"
		   "DROP(2);\n"
		   "MOV(R1,R0);\n"
		   "JUMP(continue_two_frac);\n"
		   "END_LOCAL_LABELS;"
		   "NOP;\n"
		   "L_sub_finish:\n"

		   "POP(R6);\n"
		   "POP(R5);\n"
		   "POP(R8);\n"
		   "POP(R7);\n"
		   "POP(R2);\n"
		   "POP(R1);\n"

		   "POP(FP);\n"
		   "RETURN;\n"
		   "PRIM_SUB_exit:\n")))

(define primitive-*
  (lambda ()
    (string-append "FVAR_MUL:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(5821000));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_MUL));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar *))) "),R0);\n"
		   "JUMP(PRIM_MUL_exit);\n"

		   "PRIM_MUL:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"

		   "MOV(R1,FPARG(1));\n"
		   "MOV(R2,IMM(2));\n"
		   "MOV(R3, IMM(1));\n"
		   "MOV(R4,IMM(1));\n"
		   "CMP(R1, IMM(0));\n"
		   "JUMP_EQ(mul_result);\n"
		   
		   "loop_mul:\n"
		   "CMP(R1,IMM(0));\n"
		   "JUMP_EQ(mul_result);\n"
		   "MOV(R5, FPARG(R2));\n"
		   "PUSH(R1);\n"
		   "PUSH(R2);\n"
		   "PUSH(R3);\n"
		   "PUSH(R4);\n"
		   "PUSH(R5);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "POP(R4);\n"
		   "POP(R3);\n"
		   "POP(R2);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(mul_frac);\n"
		   "MUL(R3,INDD(R5,1));\n"
		   "INCR(R2);\n"
		   "DECR(R1);\n"
		   "JUMP(loop_mul);\n"

		   "mul_frac:\n"
		   "MOV(R6,INDD(R5,1));\n"
		   "MUL(R3,INDD(R6,1));\n"
		   "MOV(R6,INDD(R5,2));\n"
		   "MUL(R4,INDD(R6,1));\n"
		   "INCR(R2);\n"
		   "DECR(R1);\n"
		   "JUMP(loop_mul);\n"

		   "mul_result:\n"
		   "PUSH(R4);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "PUSH(R0);\n"
		   "PUSH(R3);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "PUSH(R0);\n"
		   "CALL(MAKE_SOB_FRACTION);\n"
		   "DROP(2);\n"
		   "PUSH(R0);\n"
		   "CALL(SIMPLIFY_FRAC);\n"
		   "DROP(1);\n"
		   
		   "mul_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"


		   "PRIM_MUL_exit:\n"
		   )))
		   

(define primitive-apply
  (lambda ()
    (string-append "FVAR_APPLY:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(21347489));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_APPLY));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar apply))) "),R0);\n"
		   "JUMP(PRIM_APPLY_exit);\n"

		   "PRIM_APPLY:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"

		   "MOV(R3,FPARG(3));\n"
		   "MOV(R1,0);\n"
		   "L_apply_count_args:\n"

		   "PUSH(R1);\n"
		   "PUSH(R3);\n"
		   "CALL(IS_SOB_NIL);\n"
		   "POP(R3);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_EQ(L_apply_count_args_exit);\n"
		   "INCR(R1);\n"
		   "MOV(R3,INDD(R3,2));\n"
		   "JUMP(L_apply_count_args);\n"
		   "L_apply_count_args_exit:\n"

		   "SHOW(\"num of args in arg list \", R1);\n"

		   "MOV(R3,FPARG(3));\n"
		   "ADD(SP,R1);\n"

		   "MOV(R2,SP);\n"
		   "PUSH(R1);\n"
		   "DECR(R2);\n"
		   "push_args_new_frame:\n"
		   "CMP(R1,IMM(0));\n"
		   "JUMP_LE(push_args_exit);\n"
		   "MOV(STACK(R2),INDD(R3,1));\n"
		   "MOV(R3,INDD(R3,2));\n"
		   "DECR(R1);\n"
		   "DECR(R2);\n"
		   "JUMP(push_args_new_frame);\n"
		   "push_args_exit:\n"

		   #;"INFO;\n"
		   #;"SHOW(\"***************** \",R0);\n"
		   #;"MOV(R1,IMM(0));\n"
		   #;"JUMPA(R1);\n"

		   "MOV(R14,FPARG(2));\n"
		   "PUSH(INDD(R14,1));\n"
		   "PUSH(FPARG(-1));\n"
		   "MOV(R1,FPARG(-2));\n"


		   "MOV(R15,FP);\n"
		   "SUB(R15,IMM(6));\n"

		   #;"INFO;\n"

		   #;"SHOW(\"RET ADDR BEFORE TC-APPLIC\", FPARG(-1));\n"
		   #;"SHOW(\"OLD ARG COUNT m\", FPARG(1));\n"

		   "BEGIN_LOCAL_LABELS apply_loop, apply_loop_exit;\n"
		   "MOV(R2,SP);\n"
		   "SUB(R2,STARG(1));\n"
		   "SUB(R2,IMM(3));\n"

		   #;"SHOW(\"NEW FRAME\", R2);\n"
		   
		   "MOV(R3,FP);\n"
		   "SUB(R3,FPARG(1));\n"
		   "SUB(R3,4);\n"
		   "MOV(R4,IMM(0));\n"
		   "MOV(R5,SP);\n"
		   "SUB(R5,R2);\n"
		   "apply_loop:\n"
		   "CMP(R4,R5);\n"
		   "JUMP_GE(apply_loop_exit);\n"
		   "MOV(STACK(R3),STACK(R2));\n"
		   "INCR(R2);\n"
		   "INCR(R3);\n"
		   "INCR(R4);\n"
		   "JUMP(apply_loop);\n"
		   "apply_loop_exit:\n"
		   "NOP;\n"
		   "END_LOCAL_LABELS;\n"

		   #;"INFO;\n"
		   
		   "ADD(R15,R4);\n"
		   "MOV(SP,R3);\n"
		   
		   "MOV(FP,R1);\n"
		   "INFO;\n"
		   "SHOW(\" ************** \", INDD(R14,2));\n"

		   "JUMPA(INDD(R14,2));\n"

		   "PRIM_APPLY_exit:\n"
		   )))

(define primitive-boolean?
  (lambda ()
    (string-append "FVAR_BOOLPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(18420963));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_BOOLPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar boolean?))) "),R0);\n"
		   "JUMP(PRIM_BOOLPRED_exit);\n"

		   "PRIM_BOOLPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_BOOL);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_boolpred_not_bool);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_boolpred_exit);\n"
		   "L_boolpred_not_bool:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_boolpred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_BOOLPRED_exit:\n")))

(define primitive-char?
  (lambda ()
    (string-append "FVAR_CHARPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(420666));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_CHARPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar char?))) "),R0);\n"
		   "JUMP(PRIM_CHARPRED_exit);\n"

		   "PRIM_CHARPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_CHAR);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_charpred_not_char);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_charpred_exit);\n"
		   "L_charpred_not_char:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_charpred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_CHARPRED_exit:\n")))

(define primitive-integer?
  (lambda ()
    (string-append "FVAR_INTEGERPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(123456));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_INTEGERPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar integer?))) "),R0);\n"
		   "JUMP(PRIM_INTEGERPRED_exit);\n"

		   "PRIM_INTEGERPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_intpred_not_int);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_intpred_exit);\n"
		   "L_intpred_not_int:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_intpred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_INTEGERPRED_exit:\n")))

(define primitive-rational?
  (lambda ()
    (string-append "FVAR_RATIONALPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(753951));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_RATIONALPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar rational?))) "),R0);\n"
		   "JUMP(PRIM_RATIONALPRED_exit);\n"

		   "PRIM_RATIONALPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_FRACTION);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_ratpred_not_rational);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_ratpred_exit);\n"
		   "L_ratpred_not_rational:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_ratpred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_RATIONALPRED_exit:\n")))

(define primitive-procedure?
  (lambda ()
    (string-append "FVAR_PROCEDUREPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(859302));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_PROCEDUREPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar procedure?))) "),R0);\n"
		   "JUMP(PRIM_PROCEDUREPRED_exit);\n"

		   "PRIM_PROCEDUREPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_CLOSURE);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_procedurepred_not_procedure);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_procedurepred_exit);\n"
		   "L_procedurepred_not_procedure:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_procedurepred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_PROCEDUREPRED_exit:\n")))

(define primitive-zero?
  (lambda ()
    (string-append "FVAR_ZEROPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(13467961));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_ZEROPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar zero?))) "),R0);\n"
		   "JUMP(PRIM_ZEROPRED_exit);\n"

		   "PRIM_ZEROPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_zeropred_int);\n"
		   "MOV(R1,INDD(R1,0));\n"
		   "L_zeropred_int:\n"
		   "MOV(R1,INDD(R1,0));\n"
		   "CMP(R1,IMM(0));\n"
		   "JUMP_NE(L_zeropred_not_zero);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_zeropred_exit);\n"
		   "L_zeropred_not_zero:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_zeropred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_ZEROPRED_exit:\n")))

(define primitive-string?
  (lambda ()
    (string-append "FVAR_STRINGPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(19191919));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_STRINGPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar string?))) "),R0);\n"
		   "JUMP(PRIM_STRINGPRED_exit);\n"

		   "PRIM_STRINGPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_STRING);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_stringpred_not_str);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_stringpred_exit);\n"
		   "L_stringpred_not_str:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_stringpred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_STRINGPRED_exit:\n")))

(define primitive-symbol?
  (lambda ()
    (string-append "FVAR_SYMBOLPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(81816464));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_SYMBOLPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar symbol?))) "),R0);\n"
		   "JUMP(PRIM_SYMBOLPRED_exit);\n"

		   "PRIM_SYMBOLPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_SYMBOL);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_symbolpred_not_symbol);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_symbolpred_exit);\n"
		   "L_symbolpred_not_symbol:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_symbolpred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_SYMBOLPRED_exit:\n")))

(define primitive-vector?
  (lambda ()
    (string-append "FVAR_VECTORPRED:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(35435412));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_VECTORPRED));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar vector?))) "),R0);\n"
		   "JUMP(PRIM_VECTORPRED_exit);\n"

		   "PRIM_VECTORPRED:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_VECTOR);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_vectorpred_not_vector);\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_vectorpred_exit);\n"
		   "L_vectorpred_not_vector:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_vectorpred_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_VECTORPRED_exit:\n")))

(define primitive-char->integer
  (lambda ()
    (string-append "FVAR_CHAR_TO_INTEGER:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(7426589));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_CHAR_TO_INTEGER));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar char->integer))) "),R0);\n"
		   "JUMP(PRIM_CHAR_TO_INTEGER_exit);\n"

		   "PRIM_CHAR_TO_INTEGER:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "PUSH(R1);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_CHAR_TO_INTEGER_exit:\n")))

(define primitive-integer->char
  (lambda ()
    (string-append "FVAR_INTEGER_TO_CHAR:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(666666));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_INTEGER_TO_CHAR));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar integer->char))) "),R0);\n"
		   "JUMP(PRIM_INTEGER_TO_CHAR_exit);\n"

		   "PRIM_INTEGER_TO_CHAR:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "PUSH(R1);\n"
		   "CALL(MAKE_SOB_CHAR);\n"
		   "DROP(1);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_INTEGER_TO_CHAR_exit:\n")))

(define primitive-denominator
  (lambda ()
    (string-append "FVAR_DENOMINATOR:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(70775123));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_DENOMINATOR));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar denominator))) "),R0);\n"
		   "JUMP(PRIM_DENOMINATOR_exit);\n"

		   "PRIM_DENOMINATOR:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_EQ(L_denominator_integer);\n"
		   "MOV(R0,INDD(R1,2));\n"
		   "JUMP(L_denominator_exit);\n"
		   "L_denominator_integer:\n"
		   "PUSH(IMM(1));\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "L_denominator_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_DENOMINATOR_exit:\n")))

(define primitive-numerator
  (lambda ()
    (string-append "FVAR_NUMERATOR:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(1107869));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_NUMERATOR));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar numerator))) "),R0);\n"
		   "JUMP(PRIM_NUMERATOR_exit);\n"

		   "PRIM_NUMERATOR:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_EQ(L_numerator_integer);\n"
		   "MOV(R0,INDD(R1,1));\n"
		   "JUMP(L_numerator_exit);\n"
		   "L_numerator_integer:\n"
		   "MOV(R0,R1);\n"
		   "L_numerator_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_NUMERATOR_exit:\n")))

(define primitive-string-length
  (lambda ()
    (string-append "FVAR_STRING_LENGTH:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(18005555));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_STRING_LENGTH));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar string-length))) "),R0);\n"
		   "JUMP(PRIM_STRING_LENGTH_exit);\n"

		   "PRIM_STRING_LENGTH:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_STRING);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R0,INDD(R1,1));\n"
		   "PUSH(R0);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_STRING_LENGTH_exit:\n")))

(define primitive-string-ref
  (lambda ()
    (string-append "FVAR_STRING_REF:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(69006901));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_STRING_REF));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar string-ref))) "),R0);\n"
		   "JUMP(PRIM_STRING_REF_exit);\n"

		   "PRIM_STRING_REF:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_STRING);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R2,FPARG(3));\n"
		   "PUSH(R2);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "POP(R2);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "CMP(R2,INDD(R1,1));\n"
		   "JUMP_GE(L_string_length_error);\n"
		   "MOV(R3,IMM(2));\n"
		   "ADD(R3,R2);\n"
		   "MOV(R0,INDD(R1,R3));\n"
		   "PUSH(R0);\n"
		   "CALL(MAKE_SOB_CHAR);\n"
		   "DROP(1);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "L_string_length_error:\n"
		   "PUSH('S');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('T');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('R');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('_');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('R');\n"
		   "CALL(PUTCHAR);\n"
		   "DROP(5);\n"
		   "JUMP(L_clean_up);\n"

		   "PRIM_STRING_REF_exit:\n")))

(define primitive-string-set!
  (lambda ()
    (string-append "FVAR_STRING_SET_BANG:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(70776565));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_STRING_SET_BANG));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar string-set!))) "),R0);\n"
		   "JUMP(PRIM_STRING_SET_BANG_exit);\n"

		   "PRIM_STRING_SET_BANG:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(3));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_STRING);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R2,FPARG(3));\n"
		   "PUSH(R2);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "POP(R2);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "CMP(R2,INDD(R1,1));\n"
		   "JUMP_GE(L_string_ref_len_error);\n"
		   "MOV(R3,FPARG(4));\n"
		   "PUSH(R3);\n"
		   "CALL(IS_SOB_CHAR);\n"
		   "POP(R3);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R3,INDD(R3,1));\n"
		   "MOV(R4,IMM(2));\n"
		   "ADD(R4,R2);\n"
		   "MOV(INDD(R1,R4),R3);\n"
		   "MOV(R0,IMM(" (number->string void-addr) "));\n"
		   
		   "POP(FP);\n"
		   "RETURN;\n"

		   "L_string_ref_len_error:\n"
		   "PUSH('S');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('T');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('R');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('_');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('S');\n"
		   "CALL(PUTCHAR);\n"
		   "DROP(5);\n"
		   "JUMP(L_clean_up);\n"

		   "PRIM_STRING_SET_BANG_exit:\n")))

(define primitive-vector-length
  (lambda ()
    (string-append "FVAR_VECTOR_LENGTH:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(18006666));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_VECTOR_LENGTH));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar vector-length))) "),R0);\n"
		   "JUMP(PRIM_VECTOR_LENGTH_exit);\n"

		   "PRIM_VECTOR_LENGTH:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_VECTOR);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R0,INDD(R1,1));\n"
		   "PUSH(R0);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_VECTOR_LENGTH_exit:\n")))

(define primitive-vector-ref
  (lambda ()
    (string-append "FVAR_VECTOR_REF:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(69016902));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_VECTOR_REF));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar vector-ref))) "),R0);\n"
		   "JUMP(PRIM_VECTOR_REF_exit);\n"

		   "PRIM_VECTOR_REF:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_VECTOR);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R2,FPARG(3));\n"
		   "PUSH(R2);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "POP(R2);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "CMP(R2,INDD(R1,1));\n"
		   "JUMP_GE(L_vector_length_error);\n"
		   "MOV(R3,IMM(2));\n"
		   "ADD(R3,R2);\n"
		   "MOV(R0,INDD(R1,R3));\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "L_vector_length_error:\n"
		   "PUSH('V');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('E');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('C');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('_');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('R');\n"
		   "CALL(PUTCHAR);\n"
		   "DROP(5);\n"
		   "JUMP(L_clean_up);\n"

		   "PRIM_VECTOR_REF_exit:\n")))

(define primitive-vector-set!
  (lambda ()
    (string-append "FVAR_VECTOR_SET_BANG:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(70777676));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_VECTOR_SET_BANG));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar vector-set!))) "),R0);\n"
		   "JUMP(PRIM_VECTOR_SET_BANG_exit);\n"

		   "PRIM_VECTOR_SET_BANG:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(3));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_VECTOR);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R2,FPARG(3));\n"
		   "PUSH(R2);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "POP(R2);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "CMP(R2,INDD(R1,1));\n"
		   "JUMP_GE(L_vector_ref_len_error);\n"
		   "MOV(R3,FPARG(4));\n"
		   "MOV(R4,IMM(2));\n"
		   "ADD(R4,R2);\n"
		   "MOV(INDD(R1,R4),R3);\n"
		   "MOV(R0,IMM(" (number->string void-addr) "));\n"
		   
		   "POP(FP);\n"
		   "RETURN;\n"

		   "L_vector_ref_len_error:\n"
		   "PUSH('V');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('E');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('C');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('_');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('S');\n"
		   "CALL(PUTCHAR);\n"
		   "DROP(5);\n"
		   "JUMP(L_clean_up);\n"

		   "PRIM_VECTOR_SET_BANG_exit:\n")))

(define primitive-make-string
  (lambda ()
    (string-append "FVAR_MAKE_STRING:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(542070700));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_MAKE_STRING));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar make-string))) "),R0);\n"
		   "JUMP(PRIM_MAKE_STRING_exit);\n"

		   "PRIM_MAKE_STRING:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_GT(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "MOV(R3,R1);\n"
		   "JUMP_LT(L_make_string_put_init_val);\n"
		   "MOV(R2,FPARG(3));\n"
		   "PUSH(R2);\n"
		   "CALL(IS_SOB_CHAR);\n"
		   "POP(R2);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_make_string_error);\n"
		   "MOV(R2,INDD(R2,1));\n"
		   "JUMP(L_make_string_loop);\n"
		   "L_make_string_put_init_val:\n"
		   "MOV(R2,IMM(0));\n"
		   "L_make_string_loop:\n"
		   "CMP(R1,IMM(0));\n"
		   "JUMP_EQ(L_make_string_loop_exit);\n"
		   "PUSH(R2);\n"
		   "DECR(R1);\n"
		   "JUMP(L_make_string_loop);\n"
		   "L_make_string_loop_exit:\n"
		   "PUSH(R3);\n"
		   "CALL(MAKE_SOB_STRING);\n"
		   "INCR(R3);\n"
		   "DROP(R3);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "L_make_string_error:\n"
		   "PUSH('M');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('_');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('S');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('T');\n"
		   "CALL(PUTCHAR);\n"
		   "PUSH('R');\n"
		   "CALL(PUTCHAR);\n"
		   "DROP(5);\n"
		   "JUMP(L_clean_up);\n"

		   "PRIM_MAKE_STRING_exit:\n")))

(define primitive-make-vector
  (lambda ()
    (string-append "FVAR_MAKE_VECTOR:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(542181811));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_MAKE_VECTOR));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar make-vector))) "),R0);\n"
		   "JUMP(PRIM_MAKE_VECTOR_exit);\n"

		   "PRIM_MAKE_VECTOR:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_GT(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "MOV(R3,R1);\n"
		   "JUMP_LT(L_make_vector_put_init_val);\n"
		   "MOV(R2,FPARG(3));\n"
		   "JUMP(L_make_vector_loop);\n"
		   "L_make_vector_put_init_val:\n"
		   "PUSH(IMM(0));\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "MOV(R2,R0);\n"
		   "L_make_vector_loop:\n"
		   "CMP(R1,IMM(0));\n"
		   "JUMP_EQ(L_make_vector_loop_exit);\n"
		   "PUSH(R2);\n"
		   "DECR(R1);\n"
		   "JUMP(L_make_vector_loop);\n"
		   "L_make_vector_loop_exit:\n"
		   "PUSH(R3);\n"
		   "CALL(MAKE_SOB_VECTOR);\n"
		   "INCR(R3);\n"
		   "DROP(R3);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_MAKE_VECTOR_exit:\n")))

(define primitive-vector
  (lambda ()
    (string-append "FVAR_VECTOR:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(19283746));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_VECTOR));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar vector))) "),R0);\n"
		   "JUMP(PRIM_VECTOR_exit);\n"

		   "PRIM_VECTOR:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "MOV(R3,FPARG(1));\n"
		   "MOV(R2,IMM(2));\n"
		   #;"ADD(R2,R3);\n"
		   "MOV(R4,IMM(0));\n"
		   
		   
		   "L_vector_loop:\n"
		   "CMP(R4,R3);\n"
		   "JUMP_EQ(L_vector_loop_exit);\n"
		   "PUSH(FPARG(R2));\n"
		   "INCR(R2);\n"
		   "INCR(R4);\n"
		   "JUMP(L_vector_loop);\n"
		   "L_vector_loop_exit:\n"
		   "PUSH(R3);\n"
		   "CALL(MAKE_SOB_VECTOR);\n"
		   "INCR(R3);\n"
		   "DROP(R3);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_VECTOR_exit:\n")))

(define primitive-<
  (lambda ()
    (string-append "FVAR_LESS_THAN:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(115599));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_LESS_THAN));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar <))) "), R0);\n"
		   "JUMP(PRIM_LESS_THAN_exit);\n"


		   "PRIM_LESS_THAN:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "MOV(R1,FPARG(1));\n"
		   "CMP(R1,IMM(1));\n"
		   "JUMP_LT(L_cons_incorrect_args);\n"
		   "MOV(R2,IMM(2));\n"
		   "L_less_than_loop:\n"
		   "CMP(R1,IMM(1));\n"
		   "JUMP_EQ(L_less_than_success);\n"
		   "MOV(R3,FPARG(R2));\n"
		   "INCR(R2);\n"
		   "MOV(R4,FPARG(R2));\n"

		   "PUSH(R1);\n"
		   "PUSH(R2);\n"
		   
		   "PUSH(R4);\n"
		   "PUSH(R3);\n"
		   "PUSH(IMM(2));\n"
		   "PUSH(FPARG(0));\n"
		   "CALL(PRIM_SUB);\n"
		   "DROP(4);\n"
		   
		   "POP(R2);\n"
		   "POP(R1);\n"
		   "CMP(INDD(R0,0),IMM(T_INTEGER));\n"
		   "JUMP_EQ(L_lt_cmp_int);\n"
		   "MOV(R0,INDD(R0,1));\n"
		   "L_lt_cmp_int:\n"
		   "MOV(R0,INDD(R0,1));\n"
		   "CMP(R0,IMM(0));\n"
		   "JUMP_GE(L_less_than_fail);\n"
		   "DECR(R1);\n"
		   "JUMP(L_less_than_loop);\n"
		   "L_less_than_success:\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_less_than_exit);\n"
		   "L_less_than_fail:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_less_than_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_LESS_THAN_exit:\n")))



(define primitive->
  (lambda ()
    (string-append "FVAR_GREATER_THAN:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(995511));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_GREATER_THAN));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar >))) "), R0);\n"
		   "JUMP(PRIM_GREATER_THAN_exit);\n"


		   "PRIM_GREATER_THAN:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "MOV(R1,FPARG(1));\n"
		   "CMP(R1,IMM(1));\n"
		   "JUMP_LT(L_cons_incorrect_args);\n"
		   "MOV(R2,IMM(2));\n"
		   "L_greater_than_loop:\n"
		   "CMP(R1,IMM(1));\n"
		   "JUMP_EQ(L_greater_than_success);\n"
		   "MOV(R3,FPARG(R2));\n"
		   "INCR(R2);\n"
		   "MOV(R4,FPARG(R2));\n"

		   "PUSH(R1);\n"
		   "PUSH(R2);\n"
		   
		   "PUSH(R4);\n"
		   "PUSH(R3);\n"
		   "PUSH(IMM(2));\n"
		   "PUSH(FPARG(0));\n"
		   "SHOW(\"!!@@##\",R0);\n"
		   "INFO;\n"
		   "CALL(PRIM_SUB);\n"
		   "DROP(4);\n"
		   
		   "POP(R2);\n"
		   "POP(R1);\n"
		   "CMP(INDD(R0,0),IMM(T_INTEGER));\n"
		   "JUMP_EQ(L_gt_cmp_int);\n"
		   "MOV(R0,INDD(R0,1));\n"
		   "L_gt_cmp_int:\n"
		   "MOV(R0,INDD(R0,1));\n"
		   "SHOW(\"!!@@##\",R0);\n"
		   "INFO;\n"
		   "CMP(R0,IMM(0));\n"
		   "JUMP_LE(L_greater_than_fail);\n"
		   "DECR(R1);\n"
		   "JUMP(L_greater_than_loop);\n"
		   "L_greater_than_success:\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_greater_than_exit);\n"
		   "L_greater_than_fail:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_greater_than_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_GREATER_THAN_exit:\n")))

(define primitive-=
  (lambda ()
    (string-append "FVAR_NUMEQ:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(115511));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_NUMEQ));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar =))) "), R0);\n"
		   "JUMP(PRIM_NUMEQ_exit);\n"


		   "PRIM_NUMEQ:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "MOV(R1,FPARG(1));\n"
		   "CMP(R1,IMM(1));\n"
		   "JUMP_LT(L_cons_incorrect_args);\n"
		   "MOV(R2,IMM(2));\n"
		   "L_numeq_loop:\n"
		   "CMP(R1,IMM(1));\n"
		   "JUMP_EQ(L_numeq_success);\n"
		   "MOV(R3,FPARG(R2));\n"
		   "INCR(R2);\n"
		   "MOV(R4,FPARG(R2));\n"

		   "PUSH(R1);\n"
		   "PUSH(R2);\n"
		   
		   "PUSH(R4);\n"
		   "PUSH(R3);\n"
		   "PUSH(IMM(2));\n"
		   "PUSH(FPARG(0));\n"
		   "CALL(PRIM_SUB);\n"
		   "DROP(4);\n"
		   
		   "POP(R2);\n"
		   "POP(R1);\n"
		   "CMP(INDD(R0,0),IMM(T_INTEGER));\n"
		   "JUMP_EQ(L_numeq_cmp_int);\n"
		   "MOV(R0,INDD(R0,1));\n"
		   "L_numeq_cmp_int:\n"
		   "MOV(R0,INDD(R0,1));\n"
		   "CMP(R0,IMM(0));\n"
		   "JUMP_NE(L_numeq_fail);\n"
		   "DECR(R1);\n"
		   "JUMP(L_numeq_loop);\n"
		   "L_numeq_success:\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_numeq_exit);\n"
		   "L_numeq_fail:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"
		   "L_numeq_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_NUMEQ_exit:\n")))

(define primitive-/
  (lambda ()
    (string-append "FVAR_DIV:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1),IMM(1230123));\n"
		   "MOV(INDD(R0,2),LABEL(PRIM_DIV));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar /))) "), R0);\n"
		   "JUMP(PRIM_DIV_exit);\n"

		   "PRIM_DIV:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "MOV(R1,FPARG(1));\n" ;; R1 = number of args/loop counter
		   "CMP(R1,IMM(1));\n"
		   "JUMP_LT(L_cons_incorrect_args);\n"
		   "JUMP_EQ(L_div_one_over_arg);\n"
		   "MOV(R2,FPARG(2));\n" ;; R2 = dividend
		   "MOV(R3,IMM(3));\n" ;; R3 = next divisor
		   "MOV(R4,IMM(0));\n" ;; R4 = args for PRIM_MUL
		   "DECR(R1);\n"
		   "L_div_divisors_loop:\n"
		   "CMP(R1,IMM(0));\n"
		   "JUMP_EQ(L_div_divisors_loop_exit);\n"
		   "PUSH(FPARG(R3));\n"
		   "INCR(R4);\n"
		   "INCR(R3);\n"
		   "DECR(R1);\n"
		   "JUMP(L_div_divisors_loop);\n"
		   "L_div_divisors_loop_exit:\n"
		   "PUSH(R4);\n"
		   "PUSH(FPARG(0));\n"
		   "CALL(PRIM_MUL);\n"
		   "DROP(1);\n"
		   "POP(R4);\n"
		   "DROP(R4);\n"
		   "MOV(R4,R0);\n" ;;R4 = mul result
		   "PUSH(R4);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "POP(R4);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_EQ(L_div_invert_int);\n"
		   "PUSH(INDD(R4,1));\n"
		   "PUSH(INDD(R4,2));\n"
		   "CALL(MAKE_SOB_FRACTION);\n"
		   "DROP(2);\n"
		   "MOV(R3,R0);\n" ;; R3 = inverted divisor
		   "JUMP(L_div_mul_dividend);\n"
		   "L_div_invert_int:\n"
		   "PUSH(R4);\n"
		   "PUSH(IMM(1));\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "PUSH(R0);\n"
		   "CALL(MAKE_SOB_FRACTION);\n"
		   "DROP(2);\n"
		   "MOV(R3,R0);\n" ;; R3 = 1/int
		   "L_div_mul_dividend:\n"
		   "PUSH(R3);\n"
		   "PUSH(FPARG(2));\n"
		   "PUSH(IMM(2));\n"
		   "PUSH(FPARG(0));\n"
		   "CALL(PRIM_MUL);\n"
		   "DROP(4);\n"
		   "JUMP(L_div_exit);\n"
		   "L_div_one_over_arg:\n"
		   "MOV(R2,FPARG(2));\n"
		   "PUSH(R2);\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "POP(R2);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_EQ(L_div_one_over_integer);\n"
		   "PUSH(INDD(R2,1));\n"
		   "PUSH(INDD(R2,2));\n"
		   "CALL(MAKE_SOB_FRACTION);\n"
		   "DROP(2);\n" ;; R0 = 1/frac
		   "JUMP(L_div_exit);\n"
		   "L_div_one_over_integer:\n"
		   "PUSH(R2);\n"
		   "PUSH(IMM(1));\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "PUSH(R0);\n"
		   "CALL(MAKE_SOB_FRACTION);\n"
		   "DROP(2);\n" ;; R0 = 1/int
		   "PUSH(R0);\n"
		   "CALL(SIMPLIFY_FRAC);\n"
		   "DROP(1);\n"
		   "L_div_exit:\n"
		   
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_DIV_exit:\n")))


(define primitive-eq?
  (lambda ()
    (string-append "FVAR_EQ:\n"
		   
		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1), IMM(5552666));\n"
		   "MOV(INDD(R0,2), LABEL(PRIM_EQ));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar eq?))) "), R0);\n"
		   "JUMP(PRIM_EQ_exit);\n"

		   "PRIM_EQ:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "MOV(R0,IND(R1));\n"
		   "CMP(R0,IMM(T_INTEGER));\n"
		   "JUMP_EQ(L_int);\n"
		   "CMP(R0,IMM(T_FRAC));\n"
		   "JUMP_EQ(L_frac);\n"
		   "CMP(R0,IMM(T_CHAR));\n"
		   "JUMP_EQ(L_char);\n"
		   "CMP(R0,IMM(T_SYMBOL));\n"
		   "JUMP_EQ(L_symbol);\n"
		   "CMP(R0,IMM(T_VOID));\n"
		   "JUMP_EQ(L_void);\n"
		   "CMP(R0,IMM(T_NIL));\n"
		   "JUMP_EQ(L_nil);\n"
		   "CMP(R0,IMM(T_BOOL));\n"
		   "JUMP_EQ(L_bool);\n"
		   "CMP(R0,IMM(T_STRING));\n"
		   "JUMP_EQ(L_string);\n"
		   "CMP(R0,IMM(T_VECTOR));\n"
		   "JUMP_EQ(L_vector);\n"
		   "CMP(R0,IMM(T_PAIR));\n"
		   "JUMP_EQ(L_pair);\n"
		   "CMP(R0,IMM(T_CLOSURE));\n"
		   "JUMP_EQ(L_closure);\n"

		   "L_int:\n"
		   "MOV(R2,FPARG(3));\n"
		   "CMP(IND(R2),IMM(T_INTEGER));\n"
		   "JUMP_NE(L_eq_false);\n"
		   "CMP(INDD(R1,1),INDD(R2,1));\n"
		   "JUMP_EQ(L_eq_true);\n"
		   "JUMP(L_eq_false);\n"
		   "L_frac:\n"
		   "MOV(R2,FPARG(3));\n"
		   "CMP(IND(R2),IMM(T_FRAC));\n"
		   "JUMP_NE(L_eq_false);\n"
		   "MOV(R4,INDD(R1,1));\n"
		   "MOV(R5,INDD(R2,1));\n"
		   "CMP(INDD(R4,1),INDD(R5,1));\n"
		   "JUMP_NE(L_eq_false);\n"
		   "MOV(R4,INDD(R1,2));\n"
		   "MOV(R5,INDD(R2,2));\n"
		   "CMP(INDD(R4,1),INDD(R5,1));\n"
		   "JUMP_EQ(L_eq_true);\n"
		   "JUMP(L_eq_false);\n"
		   "L_char:\n"
		   "MOV(R2,FPARG(3));\n"
		   "CMP(IND(R2),IMM(T_CHAR));\n"
		   "JUMP_NE(L_eq_false);\n"
		   "CMP(INDD(R1,1),INDD(R2,1));\n"
		   "JUMP_EQ(L_eq_true);\n"
		   "JUMP(L_eq_false);\n"
		   "L_symbol:\n"
		   "MOV(R2,FPARG(3));\n"
		   "CMP(IND(R2),IMM(T_SYMBOL));\n"
		   "JUMP_NE(L_eq_false);\n"
		   "CMP(INDD(R1,1),INDD(R2,1));\n"
		   "JUMP_EQ(L_eq_true);\n"
		   "JUMP(L_eq_false);\n"
		   "L_void:\n"
		   "L_nil:\n"
		   "L_bool:\n"
		   "L_string:\n"
		   "L_vector:\n"
		   "L_pair:\n"
		   "L_closure:\n"
		   "MOV(R2,FPARG(3));\n"
		   "CMP(R1,R2);\n"
		   "JUMP_EQ(L_eq_true);\n"
		   "JUMP(L_eq_false);\n"

		   "L_eq_true:\n"
		   "MOV(R0,IMM(" (number->string bool-true-addr) "));\n"
		   "JUMP(L_eq_exit);\n"
		   "L_eq_false:\n"
		   "MOV(R0,IMM(" (number->string bool-false-addr) "));\n"

		   "L_eq_exit:\n"
		   "POP(FP);\n"
		   "RETURN;\n"
		   
		   "PRIM_EQ_exit:\n")))

(define primitive-symbol->string
  (lambda ()
    (string-append "FVAR_SYMBOL_TO_STRING:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1), IMM(6662555));\n"
		   "MOV(INDD(R0,2), LABEL(PRIM_SYMBOL_TO_STRING));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar symbol->string))) "), R0);\n"
		   "JUMP(PRIM_SYM_TO_STR_exit);\n"

		   "PRIM_SYMBOL_TO_STRING:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_SYMBOL);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R0,INDD(R1,1));\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_SYM_TO_STR_exit:\n")))

(define primitive-string->symbol
  (lambda ()
    (string-append "FVAR_STRING_TO_SYMBOL:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1), IMM(777020777));\n"
		   "MOV(INDD(R0,2), LABEL(PRIM_STRING_TO_SYMBOL));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar string->symbol))) "), R0);\n"
		   "JUMP(PRIM_STR_TO_SYM_exit);\n"

		   "PRIM_STRING_TO_SYMBOL:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   
		   "CMP(FPARG(1),IMM(1));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "MOV(R1,FPARG(2));\n"
		   "PUSH(R1);\n"
		   "CALL(IS_SOB_STRING);\n"
		   "POP(R1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "PUSH(R1);\n"
		   
		   "CALL(LOOK_UP_SYMBOL);\n"

		   "DROP(1);\n"
		   "PUSH(R0);\n"
		   "CALL(MAKE_SOB_SYMBOL);\n"
		   "DROP(1);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "LOOK_UP_SYMBOL:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "PUSH(R1);\n"
		   "PUSH(R2);\n"
		   "PUSH(R3);\n"

		   "MOV(R2,FPARG(0));\n"
		   "MOV(R1,IMM(" (number->string symbol-table-pointer) "));\n"
		   "MOV(R3,IMM(" (number->string (- symbol-table-pointer 1)) "));\n"

		   "CMP(IND(R1),IMM(0));\n"
		   "JUMP_EQ(L_look_up_loop_exit);\n"
		   "MOV(R1,IND(R1));\n"
		   
		   "L_look_up_loop:\n"
		   "CMP(R1,IMM(0));\n"
		   "JUMP_EQ(L_look_up_loop_exit);\n"

		   "PUSH(INDD(R1,0));\n"
		   "PUSH(R2);\n"
		   "SHOW(\"ASDASDADASD \",R0);\n"
		   "INFO;\n"
		   "CALL(STRING_EQUALS);\n"
		   "DROP(2);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_EQ(L_look_up_found);\n"
		   "MOV(R3,R1);\n"
		   "MOV(R1,INDD(R1,1));\n"
		   "JUMP(L_look_up_loop);\n"
		   "L_look_up_loop_exit:\n"
 		   "PUSH(IMM(2));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),R2);\n"
		   "MOV(INDD(R0,1),IMM(0));\n"
		   "MOV(INDD(R3,1),R0);\n"
		   "MOV(R0,R2);\n"
		   "JUMP(L_look_up_exit);\n"

		   "L_look_up_found:\n"
		   "MOV(R0,INDD(R1,0));\n"
		   "L_look_up_exit:\n"
		   "POP(R3);\n"
		   "POP(R2);\n"
		   "POP(R1);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "STRING_EQUALS:\n"
		   "PUSH(FP);\n"
		   "MOV(FP,SP);\n"
		   "PUSH(R1);\n"
		   "PUSH(R2);\n"
		   "PUSH(R3);\n"
		   "PUSH(R4);\n"

		   "MOV(R1,FPARG(0));\n"
		   "MOV(R2,FPARG(1));\n"
		   "CMP(INDD(R1,1),INDD(R2,1));\n"
		   "JUMP_NE(L_str_equals_false);\n"
		   "MOV(R3,IMM(1));\n"
		   "MOV(R4,IMM(2));\n"
		   "L_str_equals_loop:\n"
		   "CMP(INDD(R1,R4),INDD(R2,R4));\n"
		   "JUMP_NE(L_str_equals_false);\n"
		   "CMP(R3,INDD(R1,1));\n"
		   "JUMP_EQ(L_str_equals_true);\n"
		   "INCR(R3);\n"
		   "INCR(R4);\n"
		   "JUMP(L_str_equals_loop);\n"
		   "L_str_equals_false:\n"
		   "SHOW(\"STRING EQ FALSE \",R0);\n"
		   "INFO;\n"
		   "MOV(R0,IMM(0));\n"
		   "JUMP(L_str_equals_exit);\n"
		   "L_str_equals_true:\n"
		   "SHOW(\"STRING EQ TRUE \",R0);\n"
		   "INFO;\n"
		   "MOV(R0,IMM(1));\n"
		   "L_str_equals_exit:\n"

		   "POP(R4);\n"
		   "POP(R3);\n"
		   "POP(R2);\n"
		   "POP(R1);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_STR_TO_SYM_exit:\n")))
		   
(define primitive-remainder
  (lambda ()
    (string-append "FVAR_REMAINDER:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1), IMM(07032017));\n"
		   "MOV(INDD(R0,2), LABEL(PRIM_REMAINDER));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar remainder))) "), R0);\n"
		   "JUMP(PRIM_REMAINDER_exit);\n"

		   "PRIM_REMAINDER:\n"
		   "PUSH(FP);"
		   "MOV(FP,SP);\n"

		   "PUSH(R1);\n"
		   "CMP(FPARG(1),IMM(2));\n"
		   "JUMP_NE(L_cons_incorrect_args);\n"
		   "PUSH(FPARG(2));\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "PUSH(FPARG(3));\n"
		   "CALL(IS_SOB_INTEGER);\n"
		   "DROP(1);\n"
		   "CMP(R0,IMM(1));\n"
		   "JUMP_NE(L_prim_incorrect_type);\n"
		   "MOV(R0, FPARG(2));\n"
		   "MOV(R0, INDD(R0,1));\n"
		   "MOV(R1, FPARG(3));\n"
		   "MOV(R1, INDD(R1,1));\n"
		   "REM(R0, R1);\n"
		   "PUSH(R0);\n"
		   "CALL(MAKE_SOB_INTEGER);\n"
		   "DROP(1);\n"

		   "POP(R1);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_REMAINDER_exit:\n"
		   )))

(define primitive-list
  (lambda ()
    (string-append "FVAR_LIST:\n"

		   "PUSH(IMM(3));\n"
		   "CALL(MALLOC);\n"
		   "DROP(1);\n"
		   "MOV(INDD(R0,0),IMM(T_CLOSURE));\n"
		   "MOV(INDD(R0,1), IMM(070320171));\n"
		   "MOV(INDD(R0,2), LABEL(PRIM_LIST));\n"
		   "MOV(IND(" (number->string (lookup-var free-var-table '(fvar list))) "), R0);\n"
		   "JUMP(PRIM_LIST_exit);\n"

		   "PRIM_LIST:\n"
		   "PUSH(FP);"
		   "MOV(FP,SP);\n"

		   "PUSH(R1);\n"
		   "PUSH(R2);\n"
		   "PUSH(R3);\n"

		   "MOV(R1,FPARG(1));\n"
		   "MOV(R2,IMM(1));\n"
		   "ADD(R2,R1);\n"
		   "CALL(MAKE_SOB_NIL);\n"
		   "MOV(R3,R0);\n"
		   "loop_make_list:\n"
		   "CMP(R1,IMM(0));\n"
		   "JUMP_EQ(return_list);\n"
		   "PUSH(R3);\n"
		   "PUSH(FPARG(R2));\n"
		   "PUSH(IMM(2));\n"
		   "PUSH(FPARG(0));\n"
		   "CALL(CONS);\n"
		   "DROP(4);\n"
		   "MOV(R3,R0);\n"
		   "DECR(R1);\n"
		   "DECR(R2);\n"
		   "JUMP(loop_make_list);\n"
		   "return_list:\n"
		   "MOV(R0,R3);\n"
		   
		   "POP(R3);\n"
		   "POP(R2);\n"
		   "POP(R1);\n"
		   "POP(FP);\n"
		   "RETURN;\n"

		   "PRIM_LIST_exit:\n"
		   )))



(define lib (lambda ()
	      (string-append (primitive-cons)
			     (primitive-car)
			     (primitive-cdr)
			     (primitive-null?)
			     (primitive-set-cdr!)
			     (primitive-set-car!)
			     (primitive-not)
			     (primitive-pair?)
			     (primitive-+)
			     (primitive--)
			     (primitive-apply)
			     (primitive-boolean?)
			     (primitive-char?)
			     (primitive-integer?)
			     (primitive-procedure?)
			     (primitive-rational?)
			     (primitive-string?)
			     (primitive-symbol?)
			     (primitive-vector?)
			     (primitive-zero?)
			     (primitive-char->integer)
			     (primitive-integer->char)
			     (primitive-denominator)
			     (primitive-numerator)
			     (primitive-string-length)
			     (primitive-string-ref)
			     (primitive-string-set!)
			     (primitive-vector-length)
			     (primitive-vector-ref)
			     (primitive-vector-set!)
			     (primitive-make-string)
			     (primitive-make-vector)
			     (primitive-vector)
			     (primitive-<)
			     (primitive->)
			     (primitive-=)
			     (primitive-*)
			     (primitive-/)
			     (primitive-eq?)
			     (primitive-symbol->string)
			     (primitive-string->symbol)
			     (primitive-remainder)
			     (primitive-list)
			     )
	      ))

(define copy-const-table (lambda ()
			   (let* ((mem (fold-right append '() (map caddr global-const-table)))
				  (size (length mem))
				  (ret 'Daniel))
			     (letrec ((loop (lambda (memo)
					      (if (null? memo) "MOV(IND(0),R2);\n"
						  (string-append "MOV(INDD(R2,0),IMM("(number->string (car memo))"));\n"
								 "INCR(R2);\n"
								 (loop (cdr memo)))))))
			       (set! ret (string-append "MOV(R1,IMM("(number->string size)"));\n"
					      "ADD(R1,IMM("(number->string memory-start-address)"));\n"
					      "MOV(R2,IMM("(number->string memory-start-address)"));\n"
					      (loop mem)
					      ))
			       (set! memory-start (+ memory-start size))
			       ret			     
			       ))
			   ))

(define create-symbol-table
  (lambda ()
    (let ((n symbol-table-pointer)
	  (syms (filter (lambda (e) (symbol? (cadr e)))  global-const-table)))
      (letrec ((loop (lambda (sym)
		       (if (null? sym)
			   ""
			   (let* ((next-sym (car sym))
				  (mem-rep (caddr next-sym))
				  (rep-string (cadr mem-rep)))
			     (string-append "PUSH(IMM(2));\n"
					    "CALL(MALLOC);\n"
					    "DROP(1);\n"
					    "MOV(INDD(R0,0)," (number->string rep-string) ");\n"
					    "MOV(INDD(R0,1),R3);\n"
					    "MOV(R3,R0);\n"
					    (loop (cdr sym))))))))
	(string-append "MOV(R3,0);\n"
		       (loop syms)
		       "MOV(IND(" (number->string symbol-table-pointer) "),R3);\n"
		       ))
      )))

(define prologue (lambda (pe)
		   (string-append "#include<stdio.h>\n"
				  "#include<stdlib.h>\n\n"
				  "#define DO_SHOW 0\n\n"
				  "#include \"cisc.h\"\n\n\n"
				  "int main(){\n"
				  "START_MACHINE;\n"
				  "JUMP(CONTINUE);\n"

				  "#include \"char.lib\"\n"
				  "#include \"io.lib\"\n"
				  "#include \"math.lib\"\n"
				  "#include \"string.lib\"\n"
				  "#include \"system.lib\"\n"
				  "#include \"scheme.lib\"\n"
				  "#include \"debug_macros.h\"\n"

				  "CONTINUE:\n"

				  (copy-const-table)
				  (create-symbol-table)

				  "INFO;\n"
				  "SHOW(\"SYMBOL TABLE\", IND(5));\n"
				  

				  "ADD(IND(0),IMM("(number->string (length free-var-table))"));\n"

				  (lib)

				  #;(code-gen append1)
				  
				  "JUMP(L_start);\n"
				  
				  
				  "L_start:\n"

				  )
				  
		  ))

(define errors (lambda () (string-append "L_error_cannot_apply_non_clos:\n"

					 "SHOW(\"R0\", R0);\n"
					 
					 "PUSH('A');\n"
					 "CALL(PUTCHAR);\n"
					 "DROP(1);"
					 "CALL(NEWLINE);\n"
					 "JUMP(L_clean_up);\n"
					 "L_error_lambda_args_count:\n"
					 "PUSH('B');\n"
					 "CALL(PUTCHAR);\n"
					 "DROP(1);\n"
					 "CALL(NEWLINE);\n"
					 "L_cons_incorrect_args:\n"
					 "PUSH('C');\n"
					 "CALL(PUTCHAR);\n"
					 "DROP(1);\n"
					 "CALL(NEWLINE);\n"
					 "L_prim_incorrect_type:\n"
					 "PUSH('D');\n"
					 "CALL(PUTCHAR);\n"
					 "DROP(1);\n"
					 "CALL(NEWLINE);\n"
					 )))

(define epilogue (lambda (pe)
		   (string-append 
				  "JUMP(L_clean_up);\n"

				  (errors)
				  
				  "L_clean_up:\n"
				  "STOP_MACHINE;\n"
				  "return 0;\n"
				  "}"
				  )
		   ))

(define apply-AST (lambda (sexpr)
		    #;(pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines (parse sexpr)))))
		    (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines (parse sexpr))))))))
				
(define 1-
  (let ((my- -))
    (lambda (first . rest)
      (if (null? rest)
	  (my- 0 first)
	  (my- first (apply + rest))))))

#;(define primitives (lambda ()
		     (string-append "(define append
  (lambda lists
    (letrec ((append-lists
	      (lambda (list lists)
		(if (null? list)
		    (if (null? lists)
			'()
			(append-lists (car lists) (cdr lists)))
		    (cons (car list) (append-lists (cdr list) lists))))))
      (cond ((null? lists) lists)
	    ((not (pair? (car lists))) (car lists))
	    (else (append-lists (car lists) (cdr lists)))))))"

				    "(define +
  (let ((my+ +))
    (lambda addends
      (letrec ((loop (lambda (acc rest)
		       (if (null? rest)
			   acc
			   (loop (my+ acc (car rest)) (cdr rest))))))
	(loop 0 addends)))))"
				    "(define -
  (let ((my- -))
    (lambda (first . rest)
      (if (null? rest)
	  (my- 0 first)
          (my- first (apply + rest))))))"
				    
)))

(define primitives
  (lambda ()
    (file->string "support.scm")))

(define compile-scheme-file
  (lambda (input-file output-file)
    (let* ((source-code (file->string input-file))
	   (ready-code (string-append "(" (primitives)  source-code ")"))
	   (sexprs (test-string <sexpr> ready-code))
	   (sexprs (cadr (car sexprs)))
	   (pe (map apply-AST sexprs))
	   #;(pe (list input-file))
	   (output-stream (open-output-file output-file 'replace)))
      (letrec ((loop (lambda (exprs)
		       (let ((L-codegen (l-codegen)))
			 (if (null? exprs)
			     ""
			       (string-append (code-gen (car exprs))
					      "CMP(INDD(R0,0),IMM(T_VOID));\n"
					      "JUMP_EQ(" (symbol->string L-codegen) ");\n"
					      "PUSH(R0);\n"
					      "CALL(WRITE_SOB);\n"
					      "DROP(1);\n"
					      "CALL(NEWLINE);\n"
					      (symbol->string L-codegen) ":\n"
					      "NOP;\n"
					      (if (null? (cdr exprs))
						  ""
						  (loop (cdr exprs)))
					      ))))))
	(re-define)
	(set! global-const-table (make-const-table pe))
	(set! free-var-table (make-fvar-table pe))
	(display (string-append (prologue pe)
				#;(loop (cadr pe))
				(loop pe)
				(epilogue pe))
		 output-stream)
	(close-output-port output-stream))
      )))

(define code-gen
  (lambda (pe)
    (let ((const-table (make-const-table pe)))
      (string-append  (cond
		       ((tagged-by? pe 'const)
			(let ((addr (look-up-const global-const-table (cadr pe))))
			  (string-append "MOV(R0,IMM(" (number->string addr) "));\n"
					 ))
			)
		       ((tagged-by? pe 'if3) (handle_if pe))
		       ((tagged-by? pe 'seq) (handle_seq pe))
		       ((tagged-by? pe 'or) (handle_or pe))
		       ((tagged-by? pe 'applic) (handle_applic pe))
		       ((tagged-by? pe 'tc-applic) (handle_tc_applic pe))
		       ((tagged-by? pe 'lambda-simple) (handle_lambda_simple pe))
		       ((tagged-by? pe 'lambda-opt) (handle_lambda_opt pe))
		       ((tagged-by? pe 'lambda-var) (handle_lambda_var pe))
		       ((tagged-by? pe 'pvar) (handle_pvar_get pe))
		       ((tagged-by? pe 'bvar) (handle_bvar_get pe))
		       ((tagged-by? pe 'fvar) (handle_fvar_get pe))
		       ((tagged-by? pe 'def) (handle_define pe))
		       ((tagged-by? pe 'set) (cond ((tagged-by? (cadr pe) 'pvar) (handle_pvar_set pe))
						   ((tagged-by? (cadr pe) 'bvar) (handle_bvar_set pe))
						   ((tagged-by? (cadr pe) 'fvar) (handle_fvar_set pe))))
		       ((tagged-by? pe 'box-set) (cond ((tagged-by? (cadr pe) 'pvar) (handle_pvar_boxset pe))
						       ((tagged-by? (cadr pe) 'bvar) (handle_bvar_boxset pe))))
		       ((tagged-by? pe 'box-get) (cond ((tagged-by? (cadr pe) 'pvar) (handle_pvar_boxget pe))
						       ((tagged-by? (cadr pe) 'bvar) (handle_bvar_boxget pe))))
		       ((tagged-by? pe 'box) (handle_box pe))
		       (else ""))))))
      
