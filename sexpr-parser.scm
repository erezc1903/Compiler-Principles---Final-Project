(load "pc.scm")


(define merge-list (lambda (l)
	(cond ((null? l) '())
		  ((not (list? l)) (list l))
		  (else (append (merge-list (car l))
                        (merge-list (cdr l)))))))


(define <Whitespace> 
		(new
			(*parser (const (lambda (ch) (char<=? ch #\space))))
		done))



(define InfixContinuation (lambda (first rest) 
							(letrec ((forward (lambda (head tail)
												(if (null? tail)
												head
												(forward (list (caar tail) head (cadar tail)) (cdr tail))))))
							(forward first rest))))

(define InfixContinuationPow (lambda (first rest) 
							(letrec ((forward (lambda (head tail)
												(if (null? tail)
												head
												(forward (list (caar tail) (cadar tail) head) (cdr tail))))))
							(forward first rest))))

(define FuncArrayContinuation (lambda (first rest) 
							(letrec ((forward (lambda (head tail)
												(cond ((null? tail) head)
														((equal? (caar tail) 'vector-ref) (forward (list (caar tail) head (cadar tail)) (cdr tail)))
														(else (forward (append (list head) (car tail)) (cdr tail)))
														))))
							(forward first rest))))

(define <PrefixComment>
	(new
		(*parser (word "#;"))
		(*delayed (lambda () <sexpr>))
		(*delayed (lambda () <InfixExpression>))
		*diff
		(*caten 2)
	done))

(define <InfixComment>
	(new
		(*parser (char #\#))
    	(*parser (char #\;))
    	(*delayed (lambda () <InfixExpression>))
    	(*caten 3)	
    done))

(define <LineComment>
	(new
		(*parser (char #\;))
			
		(*parser <any>)
		(*parser (char #\newline))
		*diff
		*star
			
		(*parser (char #\newline))
		(*parser <end-of-input>)
		(*disj 2)
		(*caten 3)
	done))

(define <OffCode>
	(new
		(*parser <Whitespace>)
		(*parser <PrefixComment>)
		(*parser <InfixComment>)
		(*parser <LineComment>)
		(*disj 4)
		*star
		(*pack (lambda (ch) #\space))
	done))

;;; The definition of a boolean expression
(define <true>
    (new
		(*parser <OffCode>)
		(*parser (char #\#))
		(*parser (char-ci #\t))
 		(*caten 3)
 		(*parser (char #\.))
		(*parser (char #\)))
    	(*parser (char #\}))
		(*parser <OffCode>)
		(*parser <end-of-input>)	
        (*disj 5)
        *followed-by
        (*pack (lambda (ch) #t))
        done))

(define <false>
    (new
    	(*parser <OffCode>)
		(*parser (char #\#))
		(*parser (char-ci #\f))
 		(*caten 3)
 		(*parser (char #\.))
		(*parser (char #\)))
    	(*parser (char #\}))
		(*parser <OffCode>)
		(*parser <end-of-input>)	
        (*disj 5)
        *followed-by
        (*pack (lambda (ch) #f))
        done))

(define <Boolean> 
	(disj <true> <false>))


;;; The definition of a character

(define <CharPrefix>
		(new
			(*parser <OffCode>)
			(*parser (word "#\\"))
			(*caten 2)
			(*pack-with (lambda (ws pr) pr))
		done))


(define <VisibleSimpleChar> 
		(new
			(*parser (const (lambda (ch) (char>? ch #\space))))
			(*parser <OffCode>)
			(*caten 2)
			(*pack-with (lambda (ch ws) ch))
		done))


(define <NamedChar> 
		(new
			(*parser (word-ci "lambda"))
			(*pack (lambda (name) #\x3bb))
			(*parser (word-ci "newline"))	
			(*pack (lambda (name) #\newline))
			(*parser (word-ci "nul")) 
			(*pack (lambda (name) #\nul)) 
			(*parser (word-ci "page"))
			(*pack (lambda (name) #\page))
			(*parser (word-ci "return")) 
			(*pack (lambda (name) #\return)) 
			(*parser (word-ci "space"))
			(*pack (lambda (name) #\space)) 
			(*parser (word-ci "tab"))
			(*pack (lambda (name) #\tab))
			(*disj 7)
			(*parser <OffCode>)
			(*caten 2)
			(*pack-with (lambda (word ws) word))
		done))

(define <HexChar>
		(new 
			(*parser <OffCode>)		
			(*parser (range #\0 #\9)) 
		    (*parser (range #\a #\f))
			(*disj 2)
			(*caten 2)
			
			(*pack-with (lambda (ws1 ch) ch))
		done))

(define <HexUnicodeChar> 
	(new
		(*parser <OffCode>)
		(*parser (char-ci #\x))
		(*caten 2)
		(*pack-with (lambda (ws hexpr) hexpr))
		(*parser <HexChar>)
		*plus
		(*caten 2)
		(*pack (lambda (hex) (integer->char (string->number (list->string (cadr hex)) 16))))
	done))


(define <Char> 
	(new
		(*parser <OffCode>)
		(*parser <CharPrefix>)
		(*parser <HexUnicodeChar>)
		(*parser <NamedChar>)
		(*parser <VisibleSimpleChar>)
		(*disj 3)
		(*caten 3)
		(*pack-with (lambda (ws cpr ch) ch))
	done))


;;; The definition of a number

(define <Natural> 
	(new 
		(*parser (range #\0 #\9))
		*plus
		(*pack (lambda (number)(string->number(list->string number))))
		(*parser <end-of-input>)
		(*parser (const (lambda (ch) (or (and (char<=? ch #\9) (char>=? ch #\0)) (char<=? ch #\space) (char=? ch #\/)
		(char=? ch #\.) (char=? ch #\)) (char=? ch #\]) (char=? ch #\}) (char=? ch #\;) (char=? ch #\+) (char=? ch #\,) (char=? ch #\-) 
		(equal? ch "**") (char=? ch #\*)  (char=? ch #\^)))))
		(*disj 2)
		*followed-by
	done))

		
(define <SignedInteger> 
	(new
		(*parser (char #\+))
		(*parser (range #\0 #\9))
		*plus
		(*caten 2)
		(*parser (char #\-))
		(*parser (range #\0 #\9))
		*plus
		(*caten 2)
		(*disj 2)
		(*pack (lambda (number) (string->number(list->string (append (cons (car number) '()) (cadr number))))))
		(*parser (const (lambda (ch) (and (char>? ch #\9) (char<? ch #\0)))))
		*not-followed-by
	done))

(define <Integer>
	(disj <SignedInteger> <Natural>))

(define <Fraction> 
	(new
		(*parser <Integer>) 
		(*pack (lambda (number) (string->list (number->string number)))) 
		(*parser (char #\/))
		(*parser (plus (range #\0 #\9)))
		(*caten 3)
		(*pack (lambda (number)(string->number(list->string(merge-list number)))))
		(*parser (const (lambda (ch) (and (char>? ch #\9) (char<? ch #\0)))))
		*not-followed-by 
	done))
      

(define <Number> 
	(new
		(*parser <OffCode>)
		(*parser <Fraction>)
		(*parser <Integer>)
		(*disj 2)
		(*caten 2)
		(*pack (lambda (num) (cadr num)))
		(*parser <OffCode>)
		(*parser <end-of-input>)	
        (*disj 2)
		*followed-by
	done))

;;; The definition of a string

(define <StringHexChar> 
		(new
			(*parser (char #\\))
			(*parser (char #\x))
			(*caten 2)
			(*parser <HexChar>)
			*star
			(*parser (char #\;))
			(*caten 3)
			(*pack-with (lambda (bsx hc smc) (integer->char (string->number (list->string hc) 16))))

		done))



(define <StringMetaChar> 
		(disj
			(pack (word-ci "\\\\")(lambda (meta) #\\)) 
			(pack (word-ci "\\\"")(lambda (meta) #\")) 
			(pack (word-ci "\\t")(lambda (meta) #\tab)) 
			(pack (word-ci "\\f")(lambda (meta) #\page)) 
			(pack (word-ci "\\n")(lambda (meta) #\newline)) 
			(pack (word-ci "\\r")(lambda (meta) #\return))))

(define <StringLiteralChar>
		(new
			(*parser (const (lambda (ch) (and (not (char=? ch #\\)) (not (char=? ch #\"))))))
		done)) 
 

(define <StringChar> 
		(new
			(*parser <StringLiteralChar>)
			(*parser <StringMetaChar>)
			(*parser <StringHexChar>)
			(*disj 3)
		done))


(define <String> 
	(pack-with (caten <OffCode> (char #\") (star <StringChar>) (char #\")) 
	(lambda (ws bs1 string bs2) (list->string (merge-list string)))))


;;; The definition of a symbol

(define <SymbolChar> 
	(new
		(*parser (range #\0 #\9))
		(*parser (range #\a #\z))
		(*parser (range #\A #\Z))
		(*parser (char #\!))
		(*parser (char #\$))
		(*parser (char #\^))
		(*parser (char #\*))
		(*parser (char #\-))
		(*parser (char #\_))
		(*parser (char #\=))
		(*parser (char #\+))
		(*parser (char #\>))
		(*parser (char #\<))
		(*parser (char #\?))
		(*parser (char #\/))
		(*disj 15)
	done))

(define <Symbol> 
	(pack-with (caten <OffCode> (plus <SymbolChar>)) (lambda (ws sym) (string->symbol (string-downcase (list->string (merge-list sym)))))))


;;; The definition of a proper list

(define <ProperList> 
	(new
		(*parser <OffCode>)
		(*parser (char #\())
		(*parser <OffCode>)
		(*delayed (lambda () (^<separated-exprs> <sexpr> <OffCode>)))
		(*parser <OffCode>)
		(*parser (char #\)))
		(*caten 6)
		(*pack-with (lambda (ws1 leftParen ws2 elemList ws3 rightParen) elemList))
		(*parser <OffCode>)
		(*parser (char #\())
		(*parser <OffCode>)
		(*parser (char #\)))
		(*caten 4)
		(*pack (lambda (list) '()))
		(*disj 2)
	done))
 
;;; The definition of a improper list

(define <ImproperList> 
	(new 
		(*parser <OffCode>)
		(*parser (char #\())
		(*parser <OffCode>)  
		(*delayed (lambda () (^<separated-exprs> <sexpr> <OffCode>)))
		(*parser <OffCode>)
		(*parser (char #\.))
		(*parser <OffCode>)
		(*delayed (lambda () <sexpr>))
		(*parser <OffCode>)
		(*parser (char #\)))
		(*caten 10)
		(*pack-with (lambda (ws1 leftParen ws2 elembeforedot ws3 dot ws4 Sexpr ws5 rightParen)
					`(,@elembeforedot . ,Sexpr)))
		done)) 

;;; The definition of a vector

(define <Vector>
	(new 
		(*parser <OffCode>)
		(*parser (char #\#))
		(*parser <OffCode>)
		(*parser <ProperList>)
		(*parser <OffCode>)
		(*caten 5)
		(*pack-with (lambda (ws1 pound ws2 prList ws3) (list->vector prList)))
	done))

;;; The definition of a quoted

(define <Quoted> 
	(new
		(*parser <OffCode>)
		(*parser (char #\'))
		(*parser <OffCode>)
		(*delayed (lambda () <sexpr>))
		(*caten 4)
		(*pack-with (lambda (ws1 apostrophe ws2 Sexpr) (list 'quote Sexpr)))
	done))

;;; The definition of a quasiquoted

(define <QuasiQuoted> 
	(new
		(*parser <OffCode>)
		(*parser (char #\`))
		(*parser <OffCode>)
		(*delayed (lambda () <sexpr>))
		(*caten 4)
		(*pack-with (lambda (ws1 quasi ws2 Sexpr) (list 'quasiquote Sexpr)))
	done))

;;; The definition of a unquoted

(define <Unquoted> 
	(new
		(*parser <OffCode>)
		(*parser (char #\,))
		(*caten 2)
		(*parser (char #\@))
		*not-followed-by
		(*parser <OffCode>)
		(*delayed (lambda () <sexpr>))
		(*parser <OffCode>)
		(*caten 4)
		(*pack-with (lambda (comma ws2 Sexpr ws3) (list 'unquote Sexpr)))
	done))

;;; The definition of a unquote and spliced 

(define <UnquotedAndSpliced> 
		(new
			(*parser <OffCode>)
			(*parser (word ",@"))
			(*parser <OffCode>)
			(*delayed (lambda () <sexpr>))
			(*parser <OffCode>)
			(*caten 5)
			(*pack-with (lambda (ws1 quasi ws2 Sexpr ws3) `,@,Sexpr)) 
	done))


;;; The definition of a call-by-name

(define <CBNameSyntax1> 
		(new
			(*parser <OffCode>)
			(*parser (char #\@))
			(*parser <OffCode>)
			(*delayed (lambda () <sexpr>))
			(*caten 4)
			(*pack-with (lambda (ws1 at ws2 Sexpr)  `(cbname ,Sexpr)))
		done))

(define <CBNameSyntax2> 
		(new
			(*parser <OffCode>)
			(*parser (char #\{))
			(*parser <OffCode>)
			(*delayed (lambda () <sexpr>))
			(*parser <OffCode>)
			(*parser (char #\}))
			(*caten 6)
			(*pack-with (lambda (ws1 leftParen ws2 Sexpr ws3 rightParen)  `(cbname ,Sexpr)))
		done))

(define <CBName> 
		(new
			(*parser <CBNameSyntax1>)
			(*parser <CBNameSyntax2>)
			(*disj 2)
		done))



(define <InfixPrefixExtensionPrefix>
		(new
			(*parser (char #\#))
			(*parser (char #\#))
			(*caten 2)
			(*parser (char #\#))
			(*parser (char #\%))
			(*caten 2)
			(*disj 2)
		done))
			



(define <InfixSymbolChar> 
	(new
		(*parser (range #\0 #\9))
		(*parser (range-ci #\a #\z))
		(*parser (range-ci #\A #\Z))
		(*parser (char #\!))
		(*parser (char #\$))
		(*parser (char #\_))
		(*parser (char #\=))
		(*parser (char #\>))
		(*parser (char #\<))
		(*parser (char #\?))
		(*disj 10)
	done))


(define <InfixSymbol>
		(new
			(*parser <OffCode>)

			(*parser <InfixSymbolChar>)
			*plus

			(*parser <OffCode>)
			(*caten 3)
			(*pack-with (lambda (ws1 sym ws2) (string->symbol (string-downcase (list->string sym)))))



		done))


(define <InfixNum> 
	(new
		(*parser <OffCode>)

		(*parser <Fraction>)
		(*parser <Integer>)
		(*disj 2)
		(*delayed (lambda () <InfixSymbolChar>))
		*not-followed-by

		(*pack (lambda (num) num))

		(*parser <OffCode>)
		(*caten 3)
		(*pack-with (lambda (ws1 num ws2) num))

		(*parser <OffCode>)
		(*parser <InfixSymbol>)
		(*parser <OffCode>)
		(*caten 3)
		(*pack-with (lambda (ws1 sym ws2) sym))
    	(*disj 2)

	done))


(define <PowerSymbol>
		(new
			(*parser <OffCode>)
			(*parser (char #\^)) 
			(*parser (word "**"))
			(*disj 2)
			(*parser <OffCode>)
			(*caten 3)
			(*pack-with (lambda (ws1 sym ws2) 'expt))
		done))


(define <InfixSexprEscape> 
		(new
			(*parser <OffCode>)
			(*parser <InfixPrefixExtensionPrefix>)
			(*parser <OffCode>)
			(*delayed (lambda () <sexpr>))
			(*parser <OffCode>)
			(*caten 5)
			(*pack-with (lambda (ws1 pr ws2 Sexpr ws3)  Sexpr))

			(*parser <InfixNum>)
    		(*disj 2)
		done))


(define <InfixNeg> 
		(new
			(*parser <OffCode>)
			(*parser (char #\-))
			(*parser <OffCode>)
			(*delayed (lambda () <FuncOrArray>))
			(*parser <OffCode>)
			(*caten 5)
			(*pack-with (lambda (ws1 neg ws2 infix ws3) (if (number? infix)
														(- 0 infix)
														(list '- infix))))

			(*parser <InfixSexprEscape>)
			(*pack (lambda (ns) ns))
			(*disj 2)

		done))

(define <InfixArgList> 
		(new
			(*parser <OffCode>)
			
			(*delayed (lambda () <InfixExpression>))			

			(*parser <OffCode>)
			(*parser (char #\,))
			(*delayed (lambda () <InfixExpression>))
			(*caten 3)
			(*pack-with (lambda (ws comma infix) infix))
			*star

			(*parser <OffCode>)
			(*caten 4)


			(*pack-with (lambda (ws1 infix1 infixStar ws2)
								(append (list infix1) infixStar)))
			(*parser <epsilon>)
			(*disj 2)

		done))

(define <InfixParen> 
		(new
			(*parser <OffCode>)
			(*parser (char #\())
			(*parser <OffCode>)
			(*delayed (lambda () <InfixExpression>))
			(*parser <OffCode>)
			(*parser (char #\)))
			(*parser <OffCode>)
			(*caten 7)
			(*pack-with (lambda (ws1 leftParen ws2 infixList ws3 rightParen ws4) 
								infixList))

			(*parser <OffCode>)
			(*parser <InfixNeg>)
			(*parser <OffCode>)
			(*caten 3)
			(*pack-with (lambda (ws1 infix ws2) 
								infix))
			(*disj 2)
		done))

(define <FuncOrArray>
		(new
			(*parser <OffCode>)
			(*parser <InfixSymbol>)
			(*parser (char #\[))
			(*parser <OffCode>)
			(*delayed (lambda () <InfixExpression>))
			(*parser <OffCode>)
			(*parser (char #\]))
			(*parser <OffCode>)
			(*caten 8)
			(*pack-with (lambda (ws1 sym leftParen ws2 infix ws3 rightParen ws4)
								(list 'vector-ref sym infix)))

			(*parser <OffCode>)
			(*parser <InfixSymbol>)
			(*parser (char #\())
			(*parser <OffCode>)
			(*parser <InfixArgList>)
			(*parser <OffCode>)
			(*parser (char #\)))
			(*parser <OffCode>)
			(*caten 8)
			(*pack-with (lambda (ws1 FUNCFIRSTinfixsym leftParen ws2 FUNCFIRSTinfixList ws3 rightParen ws4)
								(append (list FUNCFIRSTinfixsym) FUNCFIRSTinfixList)))

			(*disj 2)

			(*parser <OffCode>)
			(*parser (char #\[))
			(*parser <OffCode>)
			(*delayed (lambda () <InfixExpression>))
			(*parser <OffCode>)
			(*parser (char #\]))
			(*parser <OffCode>)
			(*caten 7)
			(*pack-with (lambda (ws1 leftParen ws2 infix ws3 rightParen ws4)
						(list 'vector-ref infix)))

			(*parser <OffCode>)
			(*parser (char #\())
			(*parser <OffCode>)
			(*parser <InfixArgList>)
			(*parser <OffCode>)
			(*parser (char #\)))
			(*parser <OffCode>)
			(*caten 7)
			(*pack-with (lambda (ws1 leftParen ws2 FUNCRESTinfixList ws3 rightParen ws4)
						FUNCRESTinfixList))

			(*disj 2)
			*star
			(*caten 2)
			(*pack-with (lambda (first rest)
								(FuncArrayContinuation first rest)))

			

			(*parser <OffCode>)
			(*parser <InfixParen>)
			(*parser <OffCode>)
			(*caten 3)
			(*pack-with (lambda (ws1 infixList ws2)
						infixList))
			(*disj 2)


		done))


(define <InfixPow>
		(new
			(*parser <OffCode>)

			(*parser <FuncOrArray>)
			(*parser <OffCode>)

			(*parser <PowerSymbol>)
			(*parser <OffCode>)
			(*parser <FuncOrArray>)
			(*caten 3)
			(*pack-with (lambda (op ws infix) (list op infix)))
			*plus
			
			(*caten 3)
			(*pack-with (lambda (infix1 ws infix2) 
				(if (= (length infix2) 1)
					(InfixContinuationPow (cadar infix2) (list (list 'expt infix1)))
					(let ((first (list 'expt (cadadr (reverse infix2)) (cadar (reverse infix2))))
					  	  (rest (append (cddr (reverse infix2)) (list (list 'expt infix1)))))
						(InfixContinuationPow first rest)))))


			(*parser <OffCode>)
			(*parser <FuncOrArray>)
			(*parser <OffCode>)
			(*caten 3)
			(*pack-with (lambda (ws1 infix ws2) infix))
			
			(*disj 2)		

			(*parser <OffCode>)
			
			(*caten 3)
			(*pack-with (lambda (ws1 infix ws2) infix))

		done))



(define <InfixMulAndDiv> 
		(new
			(*parser <OffCode>)

			(*parser <InfixPow>)
			(*parser <OffCode>)

			(*parser (char #\*))
			(*parser (char #\/))
			(*disj 2)
			(*parser <OffCode>)
			(*parser <InfixPow>)
			(*caten 3)
			(*pack-with (lambda (op ws param) (if (equal? op #\*)
							    			  (list '* param) 
							    			  (list '/ param))))
			*plus

			(*caten 3)
			(*pack-with (lambda (first ws rest) 
							(InfixContinuation first rest)))

			(*parser <OffCode>)
			(*parser <InfixPow>)
			(*parser <OffCode>)
			(*caten 3)
			(*pack-with (lambda (ws1 infixPow ws2) infixPow))

			(*disj 2)		

			(*parser <OffCode>)
			
			(*caten 3)
			(*pack-with (lambda (ws1 infix ws2) infix))
		done))


(define <InfixAddAndSub> 
		(new
			(*parser <OffCode>)

			(*parser <InfixMulAndDiv>)
			(*parser <OffCode>)

			(*parser (char #\+))
			(*parser (char #\-))
			(*disj 2)
			(*parser <OffCode>)
			(*parser <InfixMulAndDiv>)
			(*caten 3)
			(*pack-with (lambda (op ws param) (if (equal? op #\+)
							    			  (list '+ param) 
							    			  (list '- param))))
			*plus

			(*caten 3)
			(*pack-with (lambda (first ws rest) 
							(InfixContinuation first rest)))

			(*parser <OffCode>)
			(*parser <InfixMulAndDiv>)
			(*parser <OffCode>)
			(*caten 3)
			(*pack-with (lambda (ws1 infixPow ws2) infixPow))

			(*disj 2)		

			(*parser <OffCode>)
			
			(*caten 3)
			(*pack-with (lambda (ws1 infix ws2) infix))
		done))


(define <InfixExpression>
		(new
			(*parser <OffCode>)
			(*parser <InfixAddAndSub>)
			(*parser <OffCode>)
			(*caten 3)
			(*pack-with (lambda (a b c) b))
		done))

(define <InfixExtension>
		(new
			(*parser <OffCode>)
			(*parser <InfixPrefixExtensionPrefix>)
			(*parser <OffCode>)
			(*delayed (lambda () <InfixExpression>))
			(*parser <OffCode>)
			(*caten 5)
			(*pack-with (lambda (ws1 prefix ws2 infixExpr ws3) infixExpr))
		done))


(define <sexpr>
		(new
			(*parser <OffCode>)
			(*parser <Boolean>)
			(*parser <Char>)
			(*parser <Number>)
			(*parser <String>)
			(*parser <Symbol>)
			(*parser <ProperList>)
			(*parser <ImproperList>)
			(*parser <Vector>)
			(*parser <Quoted>)
			(*parser <QuasiQuoted>)
			(*parser <Unquoted>)
			(*parser <UnquotedAndSpliced>)
			(*parser <CBName>)
			(*parser <InfixExtension>)
  			(*disj 14)
  			(*parser <OffCode>)
  			(*caten 3)
  			(*pack-with (lambda (oc1 exp oc2) exp))
  		done))